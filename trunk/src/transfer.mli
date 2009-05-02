(* Unison file synchronizer: src/transfer.mli *)
(* Copyright 1999-2009, Benjamin C. Pierce (see COPYING for details) *)

(*
   Rsync : general algorithm description

     The rsync algorithm is a technique for reducing the cost of a file
   transfer by avoiding the transfer of blocks that are already at the 
   destination.
     Imagine we have source and destination computers that have files X and
   Y respectively, where X and Y are similar. The algorithm proceeds as 
   follows :
   - The destination computer divides file Y into blocks of an agreed-upon 
     size N.
   - For each block, the destination computer computes two functions of the 
     block's contents :
       - A 128-bit fingerprint of the block, which with very high 
         probability is different from the fingerprints of different blocks.
       - A small checksum, which can be computed in a "rolling" fashion. 
         More precisely, if we are given the checksum for the N-byte block
         at offset k, and we are given the bytes at offsets k and N+k, we
         can efficiently compute the checksum for the N-byte block at offset
         k+1.
   - The destination computer sends a list of fingerprints and checksums to
     the source computer. Blocks are identified implicitly by the order in 
     which they appear in the list.
   - The source computer searches through file X to identify blocks that
     have the same fingerprints as blocks that appear in the list sent
     from B. The checksums are used to find candidate blocks in a single
     pass through file X. Blocks with identical fingerprints are presumed
     to be identical.
   - The source computer sends instructions for reconstructing file X at the
     destination. These instructions avoid transmitting blocks of X that are
     identical to other blocks in Y by providing the numbers of identical
     blocks and the strings containing the differences.
*)


(* Transfer instruction giving data to build a file incrementally *)
type transfer_instruction = string * int * int

type transmitter = transfer_instruction -> unit Lwt.t


(*************************************************************************)
(* GENERIC TRANSMISSION                                                  *)
(*************************************************************************)

(* Send the whole source file encoded in transfer instructions *)
val send :
    in_channel             (* source file descriptor *)
 -> Uutil.Filesize.t       (* source file length *)
 -> (int -> unit)          (* progress report *)
 -> transmitter            (* transfer instruction transmitter *)
 -> unit Lwt.t

val receive :
    out_channel            (* destination file descriptor *)
 -> (int -> unit)          (* progress report *)
 -> transfer_instruction   (* transfer instruction received *)
 -> bool                   (* Whether we have reach the end of the file *)


(*************************************************************************)
(* RSYNC TRANSMISSION                                                    *)
(*************************************************************************)

module Rsync :
  sig

    (*** DESTINATION HOST ***)

    (* The rsync compression can only be activated when the file size is
       greater than the threshold *)
    val aboveRsyncThreshold : Uutil.Filesize.t -> bool

    (* Built from the old file by the destination computer *)
    type rsync_block_info

    (* Compute block informations from the old file *)
    val rsyncPreprocess :
	   in_channel            (* old file descriptor *)
        -> rsync_block_info list

    (* Interpret a transfer instruction *)
    val rsyncDecompress :
           in_channel            (* old file descriptor *)
	-> out_channel           (* output file descriptor *)
        -> (int -> unit)         (* progress report *)
	-> transfer_instruction  (* transfer instruction received *)
	-> bool

    (*** SOURCE HOST ***)

    (* Using block informations, parse the new file and send transfer
       instructions accordingly *)
    val rsyncCompress :
	   rsync_block_info list
                              (* block info received from the destination *)
        -> in_channel         (* new file descriptor *)
        -> Uutil.Filesize.t   (* source file length *)
        -> (int -> unit)      (* progress report *)
	-> transmitter        (* transfer instruction transmitter *)
        -> unit Lwt.t

  end
