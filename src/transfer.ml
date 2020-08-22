(* Unison file synchronizer: src/transfer.ml *)
(* Copyright 1999-2020, Benjamin C. Pierce

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)


(* rsync compression algorithm

     To compress, we use a compression buffer with a size a lot
   greater than the size of a block, typically half a megabyte. This
   buffer is loaded with the file contents. Its valid part is
   represented by its limit 'length'.
     We scan the file contents by sliding a window with the size of a
   block over the compression buffer. This window is represented by
   its 'offset' and its size 'blockSize'.
     We transmit STRING tokens, containing the differences between the
   files, and BLOCK tokens, containing the number of a block from the
   old file found in the new one. The data not transmitted yet are
   pointed by 'toBeSent'.
     For each position of the window, we compute the checksum of the
   block it contains and try to find a matching entry in the hashed
   block information data. If there is a match, we compute the
   fingerprint of our block to match it with the candidates'
   fingerprints :
   - if there is a match, we've just hit, we can transmit the data not
   sent yet as a STRING token and emit a BLOCK token representing our
   match, then we slide the window one block ahead and try again;
  - in any other case, we've missed, we just slide the window one
   character ahead and try again.
     If the file size is greater than the compression buffer size,
   then we have to update the compression buffer when the window
   reaches its limit. We do so by sending any data not sent yet, then
   copying the end of the buffer at its beginning and filling it up
   with the file contents coming next. We now place our window at the
   beginning of the buffer and we continue the process.
     The compression is over when we reach the end of the file. We
   just have to send the data not sent yet together with the last
   characters that could not fill a block.  *)

let debug  = Trace.debug "transfer"
let debugV = Trace.debug "transfer+"
let debugToken = Trace.debug "rsynctoken"
let debugLog =   Trace.debug "rsynclog"

open Lwt

type transfer_instruction = Bytearray.t * int * int

type transmitter = transfer_instruction -> unit Lwt.t

(*************************************************************************)
(*                          BUFFERED DISK I/O                            *)
(*************************************************************************)

let reallyRead infd buffer pos length =
  let rec read pos length =
    let n = input infd buffer pos length in
    if n = length || n = 0 then pos + n else
    read (pos + n) (length - n)
  in
  read pos length - pos

let rec reallyWrite outfd buffer pos length =
  output outfd buffer pos length

let rec reallyWriteSubstring outfd buffer pos length =
  output_substring outfd buffer pos length

(*************************************************************************)
(*                            TOKEN QUEUE                                *)
(*************************************************************************)

(* There are two goals:
   1) to merge consecutive compatible tokens (catenating STRING tokens
      and combining BLOCK tokens when the referenced blocks are
      consecutive)
   2) to delay the transmission of the tokens across the network until
      their total size is greater than a limit, not to make a costly
      RPC for each token (therefore, the rsync module uses memory up to
      (2 * comprBufSize + tokenQueueLimit) bytes at a time) *)

type token =
  | STRING   of bytes * int * int
  | BLOCK of int
  | EOF

(* Size of a block *)
let minBlockSize = 700

(* This should at most 65535+3 bytes, as we are using this size to
   ensure that string token lengths will fit in 2 bytes. *)
let queueSize = 65500
let queueSizeFS = Uutil.Filesize.ofInt queueSize
type tokenQueue =
  { mutable data : Bytearray.t;  (* the queued tokens *)
    mutable previous : [`Str of int | `Block of int | `None];
                                 (* some information about the
                                    previous token *)
    mutable pos : int;           (* head of the queue *)
    mutable prog : int;          (* the size of the data they represent *)
    mutable bSize : int }        (* block size *)

let encodeInt3 s pos i =
  assert (i >= 0 && i < 256 * 256 * 256);
  s.{pos + 0} <- Char.chr ((i lsr 0) land 0xff);
  s.{pos + 1} <- Char.chr ((i lsr 8) land 0xff);
  s.{pos + 2} <- Char.chr ((i lsr 16) land 0xff)

let decodeInt3 s pos =
  (Char.code s.{pos + 0} lsl 0) lor
  (Char.code s.{pos + 1} lsl 8) lor
  (Char.code s.{pos + 2} lsl 16)

let encodeInt2 s pos i =
  assert (i >= 0 && i < 65536);
  s.{pos + 0} <- Char.chr ((i lsr 0) land 0xff);
  s.{pos + 1} <- Char.chr ((i lsr 8) land 0xff)

let decodeInt2 s pos =
  (Char.code s.{pos + 0} lsl 0) lor (Char.code s.{pos + 1} lsl 8)

let encodeInt1 s pos i =
  assert (i >= 0 && i < 256);
  s.{pos + 0} <- Char.chr i

let decodeInt1 s pos =
  Char.code s.{pos + 0}

(* Transmit the contents of the tokenQueue *)
let flushQueue q showProgress transmit cond =
  if cond && q.pos > 0 then begin
    debugToken (fun() -> Util.msg "flushing the token queue\n");
    transmit (q.data, 0, q.pos) >>= (fun () ->
    showProgress q.prog;
    q.pos <- 0; q.prog <- 0; q.previous <- `None;
    return ())
  end else
    return ()

let pushEOF q showProgress transmit =
  if Trace.enabled "rsynctoken" then
    debugToken (fun() ->
      Util.msg "pushing EOF (pos:%d/%d)\n" q.pos queueSize);
  flushQueue q showProgress transmit
    (q.pos + 1 > queueSize) >>= (fun () ->
  assert (q.pos < queueSize);
  q.data.{q.pos} <- 'E';
  q.pos <- q.pos + 1;
  q.previous <- `None;
  return ())

let rec pushString q id transmit s pos len =
  flushQueue q id transmit (q.pos + len + 3 > queueSize) >>= fun () ->
  if Trace.enabled "rsynctoken" then
    debugToken (fun() ->
      Util.msg "pushing string (pos:%d/%d len:%d)\n" q.pos queueSize len);
  let l = min len (queueSize - q.pos - 3) in
  assert (l > 0);
  q.data.{q.pos} <- 'S';
  encodeInt2 q.data (q.pos + 1) l;
  Bytearray.blit_from_bytes s pos q.data (q.pos + 3) l;
  q.pos <- q.pos + l + 3;
  q.prog <- q.prog + l;
  q.previous <- `Str l;
  if l < len then
    pushString q id transmit s (pos + l) (len - l)
  else
    return ()

let growString q id transmit len' s pos len =
  if Trace.enabled "rsynctoken" then
    debugToken (fun() ->
      Util.msg "growing string (pos:%d/%d len:%d+%d)\n"
        q.pos queueSize len' len);
  let l = min (queueSize - q.pos) len in
  Bytearray.blit_from_bytes s pos q.data q.pos l;
  assert (q.pos - len' - 3 >= 0);
  assert (q.data.{q.pos - len' - 3} = 'S');
  assert (decodeInt2 q.data (q.pos - len' - 2) = len');
  let len'' = len' + l in
  encodeInt2 q.data (q.pos - len' - 2) len'';
  q.pos <- q.pos + l;
  q.prog <- q.prog + l;
  q.previous <- `Str len'';
  if l < len then
    pushString q id transmit s (pos + l) (len - l)
  else
    return ()

let pushBlock q id transmit pos =
  flushQueue q id transmit (q.pos + 5 > queueSize) >>= (fun () ->
  if Trace.enabled "rsynctoken" then
    debugToken (fun() ->
      Util.msg "pushing block (pos:%d/%d)\n" q.pos queueSize);
  assert (q.pos + 5 <= queueSize);
  q.data.{q.pos} <- 'B';
  encodeInt3 q.data (q.pos + 1) pos;
  encodeInt1 q.data (q.pos + 4) 1;
  q.pos <- q.pos + 5;
  q.prog <- q.prog + q.bSize;
  q.previous <- `Block (pos + 1);
  return ())

let growBlock q id transmit pos =
  if Trace.enabled "rsynctoken" then
    debugToken (fun() ->
      Util.msg "growing blocks (pos:%d/%d)\n" q.pos queueSize);
  assert (q.pos >= 5);
  let count = decodeInt1 q.data (q.pos - 1) in
  assert (q.data.{q.pos - 5} = 'B');
  assert (decodeInt3 q.data (q.pos - 4) + count = pos);
  assert (count < 255);
  encodeInt1 q.data (q.pos - 1) (count + 1);
  q.prog <- q.prog + q.bSize;
  q.previous <- if count = 254 then `None else `Block (pos + 1);
  return ()

(* Queue a new token, possibly merging it with a previous compatible
   token and flushing the queue if its size becomes greater than the
   limit *)
let queueToken q id transmit token =
  match token, q.previous with
    EOF, _ ->
      pushEOF q id transmit
  | STRING (s, pos, len), `Str len' ->
      growString q id transmit len' s pos len
  | STRING (s, pos, len), _ ->
      pushString q id transmit s pos len
  | BLOCK pos, `Block pos' when pos = pos' ->
      growBlock q id transmit pos
  | BLOCK pos, _ ->
      pushBlock q id transmit pos

let makeQueue blockSize =
  { data =
      (* We need to make sure here that the size of the queue is not
         larger than 65538
         (1 byte: header, 2 bytes: string size, 65535 bytes: string) *)
      Bytearray.create queueSize;
    pos = 0; previous = `None; prog = 0;
    bSize = blockSize }

(*************************************************************************)
(* GENERIC TRANSMISSION                                                  *)
(*************************************************************************)

let debug = Trace.debug "generic"

(* Slice the file into STRING tokens that are transmitted incrementally *)
let send infd length showProgress transmit =
  debug (fun() -> Util.msg "sending file\n");
  let timer = Trace.startTimer "Sending file using generic transmission" in
  let bufSz = 8192 in
  let bufSzFS = Uutil.Filesize.ofInt 8192 in
  let buf = Bytes.create bufSz in
  let q = makeQueue 0 in
  let rec sendSlice length =
    if length > Uutil.Filesize.zero then begin
      let count =
        reallyRead infd buf 0
          (if length > bufSzFS then bufSz else Uutil.Filesize.toInt length) in
      if count = 0 then
        Lwt.return ()
      else begin
        queueToken q showProgress transmit (STRING (buf, 0, count))
          >>= fun () ->
        let length = Uutil.Filesize.sub length (Uutil.Filesize.ofInt count) in
        sendSlice length
      end
    end else
      Lwt.return ()
  in
  sendSlice length >>= (fun () ->
  queueToken q showProgress transmit EOF >>= (fun () ->
  flushQueue q showProgress transmit true >>= (fun () ->
  Trace.showTimer timer;
  return ())))

let rec receiveRec outfd showProgress data pos maxPos =
  if pos = maxPos then false else
  match data.{pos} with
    'S' ->
      let length = decodeInt2 data (pos + 1) in
      if Trace.enabled "generic" then debug (fun() -> Util.msg
          "receiving %d bytes\n" length);
      reallyWriteSubstring outfd (Bytearray.sub data (pos + 3) length) 0 length;
      showProgress length;
      receiveRec outfd showProgress data (pos + length + 3) maxPos
  | 'E' ->
      true
  | _   ->
      assert false

let receive outfd showProgress (data, pos, len) =
  receiveRec outfd showProgress data pos (pos + len)

(*************************************************************************)
(* RSYNC TRANSMISSION                                                    *)
(*************************************************************************)

module Rsync =
struct

  (* Debug messages *)
  let debug =      Trace.debug "rsync"


(**************************** DESTINATION HOST ***************************)

  (* It is impossible to use rsync when the file size is smaller than
     the size of a block *)
  let minBlockSizeFs = Uutil.Filesize.ofInt minBlockSize
  let aboveRsyncThreshold sz = sz > minBlockSizeFs

  (* The type of the info that will be sent to the source host *)
  type rsync_block_info =
    { blockSize : int;
      blockCount : int;
      checksumSize : int;
      weakChecksum :
        (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t;
      strongChecksum : Bytearray.t }

  (*** PREPROCESS ***)

  (* Worst case probability of a failure *)
  let logProba = -27. (* One time in 100 millions *)
  (* Strength of the weak checksum
     (how many bit of the weak checksum we can rely on) *)
  let weakLen = 27.
  (* This is what rsync uses:
       let logProba = -10.
       let weakLen = 31.
     This would save almost 3 bytes per block, but one need to be able
     to recover from an rsync error.
     (We would have to take into account that our weak checksum is
      only 31 bits.)
  *)
  (* Block size *)
  let computeBlockSize l = truncate (max 700. (min (sqrt l) 131072.))
  (* Size of each strong checksum *)
  let checksumSize bs sl dl =
    let bits =
      -. logProba -. weakLen +. log (sl *. dl /. float bs) /. log 2. in
    max 2 (min 16 (truncate ((bits +. 7.99) /. 8.)))

  let sizes srcLength dstLength =
    let blockSize = computeBlockSize (Uutil.Filesize.toFloat dstLength) in
    let blockCount =
      let count =
        Int64.div (Uutil.Filesize.toInt64 dstLength) (Int64.of_int blockSize)
      in
      Int64.to_int (min 16777216L count)
    in
    let csSize =
      checksumSize blockSize
        (Uutil.Filesize.toFloat srcLength)(Uutil.Filesize.toFloat dstLength)
    in
    (blockSize, blockCount, csSize)

  (* Incrementally build arg by executing f on successive blocks (of size
     'blockSize') of the input stream (pointed by 'infd').
     The procedure uses a buffer of size 'bufferSize' to load the input,
     and eventually handles the buffer update. *)
  let blockIter infd f blockSize maxCount =
    let bufferSize = 8192 + blockSize in
    let buffer = Bytes.create bufferSize in
    let rec iter count offset length =
      if count = maxCount then
        count
      else begin
        let newOffset = offset + blockSize in
        if newOffset <= length then begin
          f count buffer offset;
          iter (count + 1) newOffset length
        end else if offset > 0 then begin
          let chunkSize = length - offset in
          Bytes.blit buffer offset buffer 0 chunkSize;
          iter count 0 chunkSize
        end else begin
          let l = input infd buffer length (bufferSize - length) in
          if l = 0 then
            count
          else
            iter count 0 (length + l)
        end
      end
    in
    iter 0 0 0

  (* Given a block size, get blocks from the old file and compute a
     checksum and a fingerprint for each one. *)
  let rsyncPreprocess infd srcLength dstLength =
    debug (fun() -> Util.msg "preprocessing\n");
    let (blockSize, blockCount, csSize) = sizes srcLength dstLength in
    debugLog (fun() ->
      Util.msg "block size = %d bytes; block count = %d; \
                strong checksum size = %d\n" blockSize blockCount csSize);
    let timer = Trace.startTimer "Preprocessing old file" in
    let weakCs =
      Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout blockCount in
    let strongCs = Bytearray.create (blockCount * csSize) in
    let addBlock i buf offset =
      weakCs.{i} <- Int32.of_int (Checksum.subbytes buf offset blockSize);
      Bytearray.blit_from_string
        (Digest.subbytes buf offset blockSize) 0 strongCs (i * csSize) csSize
    in
    (* Make sure we are at the beginning of the file
       (important for AppleDouble files *)
    LargeFile.seek_in infd 0L;
    let count =
      (* Limit the number of blocks so that there is no overflow in
         encodeInt3 *)
      blockIter infd addBlock blockSize (min blockCount (256*256*256)) in
    debugLog (fun() -> Util.msg "%d blocks\n" count);
    Trace.showTimer timer;
    let sigs =
      { blockSize = blockSize; blockCount = count; checksumSize = csSize;
        weakChecksum = weakCs; strongChecksum = strongCs } in
    if
      sigs.blockCount > Bigarray.Array1.dim sigs.weakChecksum ||
      sigs.blockCount * sigs.checksumSize >
      Bigarray.Array1.dim sigs.strongChecksum
    then
      raise
        (Util.Transient
           (Format.sprintf
              "Internal error during rsync transfer (preprocess), \
               please report: %d %d - %d %d"
              sigs.blockCount (Bigarray.Array1.dim sigs.weakChecksum)
              (sigs.blockCount * sigs.checksumSize)
              (Bigarray.Array1.dim sigs.strongChecksum)));
    (sigs, blockSize)

  (* Expected size of the [rsync_block_info] datastructure (in KiB). *)
  let memoryFootprint srcLength dstLength =
    let (blockSize, blockCount, csSize) = sizes srcLength dstLength in
    blockCount * (csSize + 4)

  (*** DECOMPRESSION ***)

  (* Decompression buffer size *)
  let decomprBufSize = 8192

  (* For each transfer instruction, either output a string or copy one or
     several blocks from the old file. *)
  let rsyncDecompress blockSize infd outfd showProgress (data, pos, len) =
    let decomprBuf = Bytes.create decomprBufSize in
    let progress = ref 0 in
    let rec copy length =
      if length > decomprBufSize then begin
        let _ = reallyRead infd decomprBuf 0 decomprBufSize in
        reallyWrite outfd decomprBuf 0 decomprBufSize;
        copy (length - decomprBufSize)
      end else
        let _ = reallyRead infd decomprBuf 0 length in
        reallyWrite outfd decomprBuf 0 length
    in
    let copyBlocks n k =
      LargeFile.seek_in infd (Int64.mul n (Int64.of_int blockSize));
      let length = k * blockSize in
      copy length;
      progress := !progress + length
    in
    let maxPos = pos + len in
    let rec decode pos =
      if pos = maxPos then false else
      match data.{pos} with
        'S' ->
          let length = decodeInt2 data (pos + 1) in
          if Trace.enabled "rsynctoken" then
            debugToken (fun() ->
              Util.msg "decompressing string (%d bytes)\n" length);
          reallyWriteSubstring outfd (Bytearray.sub data (pos + 3) length) 0 length;
          progress := !progress + length;
          decode (pos + length + 3)
      | 'B' ->
          let n = decodeInt3 data (pos + 1) in
          let k = decodeInt1 data (pos + 4) in
          if Trace.enabled "rsynctoken" then
            debugToken (fun() -> Util.msg
                "decompressing %d block(s) (sequence %d->%d)\n"
                k n (n + k - 1));
          copyBlocks (Int64.of_int n) k;
          decode (pos + 5)
      | 'E' ->
          true
      | _ ->
          assert false
    in
    let finished = decode pos in
    showProgress !progress;
    finished

(***************************** SOURCE HOST *******************************)

  (*** CUSTOM HASH TABLE ***)

  (* Half the maximum number of entries in the hash table.
     MUST be a power of 2 !
     Typical values are around an average 2 * fileSize / blockSize. *)
  let hashTableMaxLength = 1024 * 1024

  let rec upperPowerOfTwo n n2 =
    if (n2 >= n) || (n2 = hashTableMaxLength) then
      n2
    else
      upperPowerOfTwo n (2 * n2)

  let hash checksum = checksum

  (* Compute the hash table length as a function of the number of blocks *)
  let computeHashTableLength signatures =
    2 * (upperPowerOfTwo signatures.blockCount 32)

  (* Hash the block signatures into the hash table *)
  let hashSig hashTableLength signatures =
    let hashTable = Array.make hashTableLength [] in
    for k = 0 to signatures.blockCount - 1 do
      let cs = Int32.to_int signatures.weakChecksum.{k} land 0x7fffffff in
      let h = (hash cs) land (hashTableLength - 1) in
      hashTable.(h) <- (k, cs) :: hashTable.(h)
    done;
    hashTable

  (* Given a key, retrieve the corresponding entry in the table *)
  let findEntry hashTable hashTableLength checksum :
      (int * Checksum.t) list =
    let i = (hash checksum) land (hashTableLength - 1) in
    hashTable.(i)

  let sigFilter hashTableLength signatures =
    let len = hashTableLength lsl 2 in
    let filter = Bytes.make len '\000' in
    for k = 0 to signatures.blockCount - 1 do
      let cs = Int32.to_int signatures.weakChecksum.{k} land 0x7fffffff in
      let h1 = cs lsr 28 in
      assert (h1 >= 0 && h1 < 8);
      let h2 = (cs lsr 5) land (len - 1) in
      let mask = 1 lsl h1 in
      Bytes.set filter h2 (Char.chr (Char.code (Bytes.get filter h2) lor mask))
    done;
    Bytes.to_string filter

  let filterMem filter hashTableLength checksum =
    let len = hashTableLength lsl 2 in
    let h2 = (checksum lsr 5) land (len - 1) in
    let h1 = checksum lsr 28 in
    let mask = 1 lsl h1 in
    Char.code (String.unsafe_get filter h2) land mask <> 0

  (* Log the values of the parameters associated with the hash table *)
  let logHash hashTable hashTableLength =
    let rec probe empty collision i =
      if i = hashTableLength then (empty, collision)
      else begin
        let length = Safelist.length hashTable.(i) in
        let next =
          if length = 0 then probe (empty + 1) collision
          else if length > 1 then probe empty (collision + 1)
          else probe empty collision
        in
        next (i + 1)
      end
    in
    let (empty, collision) = probe 0 0 0 in
    debugLog (fun() -> Util.msg "%d hash table entries\n" hashTableLength);
    debugLog (fun() -> Util.msg
        "%d empty, %d used, %d collided\n"
        empty (hashTableLength - empty) collision)

  (*** MEASURES ***)

  type probes = {
      mutable hitHit : int;
      mutable hitMiss : int;
      mutable missMiss : int;
      mutable nbBlock : int;
      mutable nbString : int;
      mutable stringSize : int
    }

  let logMeasures pb =
    debugLog (fun() -> Util.msg
        "hit-hit = %d, hit-miss = %d, miss-miss = %d, hit rate = %d%%\n"
        pb.hitHit pb.hitMiss pb.missMiss
        (if pb.hitHit <> 0 then
           pb.hitHit * 100 / (pb.hitHit + pb.hitMiss)
         else 0))
(*
    debugLog (fun() -> Util.msg
        "%d strings (%d bytes), %d blocks\n"
        pb.nbString pb.stringSize pb.nbBlock);
    let generic = pb.stringSize + pb.nbBlock * blockSize in
    debugLog (fun() -> Util.msg
        "file size = %d bytes\n"
        generic);
    debug (fun() -> Util.msg
        "compression rate = %d%%\n" ((pb.stringSize * 100) / generic))
*)


  (*** COMPRESSION ***)

  (* Compression buffer size *)
  (* MUST be >= 2 * blockSize *)
  let minComprBufSize = 8192

  type compressorState =
    { (* Rolling checksum data *)
      mutable checksum : int;
      mutable cksumOutgoing : char;
      (* Buffering *)
      mutable offset : int;
      mutable toBeSent : int;
      mutable length : int;
      (* Position in file *)
      mutable absolutePos : Uutil.Filesize.t }

  (* Compress the file using the algorithm described in the header *)
  let rsyncCompress sigs infd srcLength showProgress transmit =
    debug (fun() -> Util.msg "compressing\n");
    if
      sigs.blockCount > Bigarray.Array1.dim sigs.weakChecksum ||
      sigs.blockCount * sigs.checksumSize >
      Bigarray.Array1.dim sigs.strongChecksum
    then
      raise
        (Util.Transient
           (Format.sprintf
              "Internal error during rsync transfer (compression), \
               please report: %d %d - %d %d"
              sigs.blockCount (Bigarray.Array1.dim sigs.weakChecksum)
              (sigs.blockCount * sigs.checksumSize)
              (Bigarray.Array1.dim sigs.strongChecksum)));
    let blockSize = sigs.blockSize in
    let comprBufSize = (2 * blockSize + 8191) land (-8192) in
    let comprBufSizeFS = Uutil.Filesize.ofInt comprBufSize in
    debugLog (fun() -> Util.msg
        "compression buffer size = %d bytes\n" comprBufSize);
    debugLog (fun() -> Util.msg "block size = %d bytes\n" blockSize);
    assert (comprBufSize >= 2 * blockSize);
    let timer = Trace.startTimer "Compressing the new file" in

    (* Measures *)
    let pb =
      { hitHit = 0; hitMiss = 0; missMiss = 0;
        nbBlock = 0; nbString = 0; stringSize = 0 } in
(*
    let transmit tokenList =
      Safelist.iter
        (fun token ->
           match token with
           | STRING s ->
               let length = String.length s in
               if Trace.enabled "rsynctoken" then debugToken (fun() ->
                 Util.msg "transmitting string (%d bytes)\n" length);
               pb.nbString <- pb.nbString + 1;
               pb.stringSize <- pb.stringSize + length
           | BLOCK n ->
               if Trace.enabled "rsynctoken" then debugToken (fun() -> Util.msg
                   "transmitting %d block(s) (sequence %d->%d)\n"
                   1 n (n));
               pb.nbBlock <- pb.nbBlock + k)
        tokenList;
      transmit tokenList
    in
*)

    (* Enable token buffering *)
    let tokenQueue = makeQueue blockSize in
    let flushTokenQueue () =
      flushQueue tokenQueue showProgress transmit true in
    let transmit token = queueToken tokenQueue showProgress transmit token in

    (* Set up the hash table for fast checksum look-up *)
    let hashTableLength = computeHashTableLength sigs in
    let blockTable = hashSig hashTableLength sigs in
    logHash blockTable hashTableLength;

    let filter = sigFilter hashTableLength sigs in

    let rec fingerprintMatchRec checksums pos fp i =
      let i = i - 1 in
      i < 0 ||
      (fp.[i] = checksums.{pos + i} &&
       fingerprintMatchRec checksums pos fp i)
    in
    let fingerprintMatch k fp =
      let pos = k * sigs.checksumSize in
      (*FIX: temporary debugging code... *)
      if
        pos + sigs.checksumSize > Bigarray.Array1.dim sigs.strongChecksum
      then
        raise
          (Util.Transient
             (Format.sprintf "Internal error during rsync transfer, \
                              please report: \
                              k:%d/%d pos:%d csSize:%d dim:%d"
                k sigs.blockCount pos sigs.checksumSize
                (Bigarray.Array1.dim sigs.strongChecksum)));
      fingerprintMatchRec sigs.strongChecksum pos fp sigs.checksumSize
    in

    (* Create the compression buffer *)
    let comprBuf = Bytes.create comprBufSize in

    (* If there is data waiting to be sent, transmit it as a STRING token *)
    let transmitString toBeSent offset =
      if offset > toBeSent then
        transmit (STRING (comprBuf, toBeSent, offset - toBeSent))
      else
        return ()
    in

    (* Set up the rolling checksum data *)
    let cksumTable = Checksum.init blockSize in

    let initialState =
      { checksum = 0; cksumOutgoing = ' ';
        offset = comprBufSize; toBeSent = comprBufSize; length = comprBufSize;
        absolutePos = Uutil.Filesize.zero }
    in

    (* Check the new window position and update the compression buffer
       if its end has been reached *)
    let rec slideWindow st miss : unit Lwt.t =
      if st.offset + blockSize <= st.length then
        computeChecksum st miss
      else if st.length = comprBufSize then begin
        transmitString st.toBeSent st.offset >>= (fun () ->
        let chunkSize = st.length - st.offset in
        if chunkSize > 0 then begin
          assert(comprBufSize >= blockSize);
          Bytes.blit comprBuf st.offset comprBuf 0 chunkSize
        end;
        let rem = Uutil.Filesize.sub srcLength st.absolutePos in
        let avail = comprBufSize - chunkSize in
        let l =
          reallyRead infd comprBuf chunkSize
            (if rem > comprBufSizeFS then avail else
             min (Uutil.Filesize.toInt rem) avail)
        in
        st.absolutePos <-
          Uutil.Filesize.add st.absolutePos (Uutil.Filesize.ofInt l);
        st.offset <- 0;
        st.toBeSent <- 0;
        st.length <- chunkSize + l;
        debugToken (fun() -> Util.msg "updating the compression buffer\n");
        debugToken (fun() -> Util.msg "new length = %d bytes\n" st.length);
        slideWindow st miss)
      end else
        transmitString st.toBeSent st.length >>= (fun () ->
        transmit EOF)

    (* Compute the window contents checksum, in a rolling fashion if there
       was a miss *)
    and computeChecksum st miss =
      if miss then
        rollChecksum st
      else begin
        let cksum = Checksum.subbytes comprBuf st.offset blockSize in
        st.checksum <- cksum;
        st.cksumOutgoing <- Bytes.unsafe_get comprBuf st.offset;
        processBlock st
      end

    and rollChecksum st =
      let ingoingChar =
        Bytes.unsafe_get comprBuf (st.offset + blockSize - 1) in
      let cksum =
        Checksum.roll cksumTable st.checksum st.cksumOutgoing ingoingChar in
      st.checksum <- cksum;
      st.cksumOutgoing <- Bytes.unsafe_get comprBuf st.offset;
      if filterMem filter hashTableLength cksum then
        processBlock st
      else
        miss st

    (* Try to match the current block with one existing in the old file *)
    and processBlock st =
      let checksum = st.checksum in
      match findEntry blockTable hashTableLength checksum with
      | [] ->
          pb.missMiss <- pb.missMiss + 1;
          miss st
      | entry ->
          let blockNum = findBlock st checksum entry None in
          if blockNum = -1 then begin
              pb.hitMiss <- pb.hitMiss + 1;
              miss st
          end else begin
              pb.hitHit <- pb.hitHit + 1;
              hit st blockNum
          end

    (* In the hash table entry, find nodes with the right checksum and
       match fingerprints *)
    and findBlock st checksum entry fingerprint =
      match entry, fingerprint with
      | [], _ ->
          -1
      | (k, cs) :: tl, None
        when cs = checksum ->
          let fingerprint = Digest.subbytes comprBuf st.offset blockSize in
          findBlock st checksum entry (Some fingerprint)
      | (k, cs) :: tl, Some fingerprint
        when cs = checksum && fingerprintMatch k fingerprint ->
          k
      | _ :: tl, _ ->
          findBlock st checksum tl fingerprint

    (* Miss : slide the window one character ahead *)
    and miss st =
      st.offset <- st.offset + 1;
      if st.offset + blockSize <= st.length then
        rollChecksum st
      else
        slideWindow st true

    (* Hit : send the data waiting and a BLOCK token, then slide the window
       one block ahead *)
    and hit st blockNum =
      transmitString st.toBeSent st.offset >>= (fun () ->
      let sent = st.offset in
      st.toBeSent <- sent + blockSize;
      transmit (BLOCK blockNum) >>= (fun () ->
      st.offset <- st.offset + blockSize;
      slideWindow st false))
    in

    (* Initialization and termination *)
    slideWindow initialState false >>= (fun () ->
    flushTokenQueue () >>= (fun () ->
    logMeasures pb;
    Trace.showTimer timer;
    return ()))

end
