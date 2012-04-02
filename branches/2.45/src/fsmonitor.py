#!/usr/bin/python

# a small program to test the possibilities to monitor the file system and 
# log changes on Windowsm Linux, and OSX
#
# Originally written by Christoph Gohle (2010) 
# Modified by Gene Horodecki for Windows 
# Further modified by Benjamin Pierce
# should be distributed under GPL

import sys
import os
import stat
from optparse import OptionParser
from time import time

def mydebug(fmt, *args, **kwds):
    if not op.debug:
        return

    if args:
        fmt = fmt % args

    elif kwds:
        fmt = fmt % kwds

    print >>sys.stderr, fmt

def mymesg(fmt, *args, **kwds):
    if not op.verbose:
        return

    if args:
        fmt = fmt % args

    elif kwds:
        fmt = fmt % kwds

    print >>sys.stdout, fmt

def timer_callback(timer, streamRef):
    mydebug("CFAbsoluteTimeGetCurrent() => %.3f", CFAbsoluteTimeGetCurrent())
    mydebug("FSEventStreamFlushAsync(streamRef = %s)", streamRef)
    FSEventStreamFlushAsync(streamRef)

def update_changes(result):
    mydebug('Update_changes: absresult = %s',result)
    #print('absresult',result)
    result = [mangle_filename(path) for path in result]
    mydebug('Update_changes: mangled = %s',result)
    #print('magnled', result)
    result = [relpath(op.root,path) for path in result]
    #print('relative to root',result)
    mydebug('Update_changes: relative to root = %s',result)

    try:
        f = open(op.absoutfile,'a')
        for path in result:
            f.write(path+'\n')
        f.close()
    except IOError:
        mymesg('failed to open log file %s for writing',op.outfile)
		
def update_changes_nomangle(result):
    # In win32 there are no symlinks, therefore file mangling
	# is not required

    # remove root from the path:
    result = relpath(op.root,result)

    mydebug('Changed paths: %s\n',result)
    try:
        # Windows hack: open in binary mode
        f = open(op.absoutfile,'ab')
        f.write(result+'\n')
        f.close()
    except IOError:
        mymesg('failed to open log file %s for writing',op.outfile)
	
def mangle_filename(path):
    """because the FSEvents system returns 'real' paths we have to figure out
if they have been aliased by a symlink and a 'follow' directive in the unison
configuration or from the command line.
This is done here for path. The return value is the path name using symlinks
"""
    try:
        op.symlinks
    except AttributeError:
        make_symlinks()
    #now lets do it
    result = path
    for key in op.symlinks:
        #print path, key
        if path.startswith(key):
            result = os.path.join(op.root,os.path.join(op.symlinks[key]+path[len(key):]))
            #print 'Match!', result
	
    return result

def make_symlinks():
    #lets create a dictionary of symlinks that are treated transparently here
    op.symlinks = {}
    fl = op.follow
    try:
        foll = [f.split(' ',1) for f in fl]
    except TypeError:
        foll = []
    for k,v in foll:
        if not k=='Path':
            mymesg('We don\'t support anything but path specifications in follow directives. Especially not %s',k)
        else:
            p = v.strip('{}')
            if not p[-1]=='/':
                p+='/'
            op.symlinks[os.path.realpath(os.path.join(op.root,p))]=p
    mydebug('make_symlinks: symlinks to follow %s',op.symlinks)
			
	
def relpath(root,path):
    """returns the path relative to root (which should be absolute)
    if it is not a path below root or if root is not absolute it returns None
    """

    if not os.path.isabs(root):
        return None
	
    abspath = os.path.abspath(path)
    mydebug('relpath: abspath(%s) = %s', path, abspath)
	
    # make sure the root and abspath both end with a '/' or '\'
    if sys.platform == 'win32':
      slash = '\\'
    else:
      slash = '/'
    
    if not root[-1]==slash:
      root += slash
    if not abspath[-1]==slash:
      abspath += slash
		
    mydebug('relpath: root = %s', root)

    #print root, abspath
    if not abspath[:len(root)]==root:
        #print abspath[:len(root)], root
        return None
    mydebug('relpath: relpath = %s',abspath[len(root):])
    return abspath[len(root):]
	
def my_abspath(path):
	"""expand path including shell variables and homedir 
to the absolute path
"""
	return os.path.abspath(os.path.expanduser(os.path.expandvars(path)))
 
def update_follow(path):
    """ tries to find a follow directive that matches path 
    and if path refers to a symbolic link the real path of the symbolic
    link is returned. """
    try:
        op.symlinks
    except AttributeError:
        make_symlinks()
    rpath = relpath(op.root, path)
    mydebug('update_follow: rpath %s', rpath)
    result = None
    foll = None
    for k in op.symlinks:
        v = op.symlinks[k]        
        if v==rpath:
            result = os.path.realpath(os.path.abspath(path))
            foll = v
            mydebug('update_follow: link %s, real %s',v,result)
            break
    if result:
        op.symlinks[result] = foll
    
    return result, foll 

def conf_parser(conffilepath, delimiter = '=', dic = {}):
	"""parse the unison configuration file at conffilename and populate a dictionary
with configuration options from there. If dic is a dictionary, these options are added to this
one (can be used to recursively call this function for include statements)."""
	try:
		conffile = open(conffilepath,'r')
	except IOError:
		mydebug('could not open configuration file at %s',conffilepath)
		return None
		
	res = dic
	
	for line in conffile:
		line = line.strip()
		if len(line)<1 or line[0]=='#':
			continue
		elif line.startswith('include'):
			dn = os.path.dirname(conffilepath)
			fn = line.split()[1].strip()
			conf_parser(os.path.join(dn,fn), dic = res)
		else:
			k,v=[s.strip() for s in line.split('=',1)]
			if res.has_key(k):
				res[k].append(v)
			else:
				res[k]=[v]
	return res

################################################
# Linux specific code here
################################################
if sys.platform.startswith('linux'):
    import pyinotify

    class HandleEvents(pyinotify.ProcessEvent):
        wm = None

        #def process_IN_CREATE(self, event):
        #    print "Creating:", event.pathname

        #def process_IN_DELETE(self, event):
        #    print "Removing:", event.pathname

        #def process_IN_MODIFY(self, event):
        #    print "Modifying:", event.pathname
            
    #    def process_IN_MOVED_TO(self, event):
    #        print "Moved to:", event.pathname
                    
    #    def process_IN_MOVED_FROM(self, event):
    #        print "Moved from:", event.pathname

    #    def process_IN_ATTRIB(self, event):
    #        print "attributes:", event.pathname

        def process_default(self, event):
            mydebug('process_default: event %s', event)
# code for adding dirs is obsolete since there is the auto_add option
#            if event.dir:
#                if event.mask&pyinotify.IN_CREATE:
#                    print 'create:', event.pathname , self.add_watch(event.pathname,rec=True)
#                elif event.mask&pyinotify.IN_DELETE:
#                    print 'remove', event.pathname, self.remove_watch(event.pathname)
#                    pass
#                elif event.mask&pyinotify.IN_MOVED_FROM:
#                    print 'move from', event.pathname, self.remove_watch(event.pathname, rec=True)
#                    pass
#                elif event.mask&pyinotify.IN_MOVED_TO:
#                    print 'move to', event.pathname, self.add_watch(event.pathname,rec=True)
#                else:
#                    pass
            #handle creation of links that should be followed            
            if os.path.islink(event.pathname):
                #special handling for links
                mydebug('process_default: link %s created/changed. Checking for follows', event.pathname)
                p, l = update_follow(event.pathname)
                if p:
                    self.add_watch(p,rec=True,auto_add=True)
                    mydebug('process_default: follow link %s to %s',l,p)
            #TODO: should handle deletion of links that are followed (delete the respective watches)
            update_changes([event.pathname])

        def remove_watch(self, pathname, **kwargs):
            if self.watches.has_key(pathname):
                return self.wm.rm_watch(self.watches.pop(pathname),**kwargs)
            return None

        def add_watch(self, pathname, **kwargs):
            neww = self.wm.add_watch(pathname, self.mask, **kwargs)
            self.watches.update(neww)
            return neww

        def init_watches(self, abspaths, follows):
            self.watches = {}
            for abspath in abspaths:
                self.watches.update(self.wm.add_watch(abspath,self.mask,rec=True,auto_add=True))
            #we have to add watches for follow statements since pyinotify does
            #not do recursion across symlinks
            make_symlinks()
            for link in op.symlinks:
                mydebug('following symbolic link %s',link)
                if not self.watches.has_key(link):
                    self.watches.update(self.wm.add_watch(link,self.mask,rec=True,auto_add=True))
                                
            mydebug('init_watches: added paths %s\n  based on paths %s\n   and follows %s',self.watches,op.abspaths, op.follow)
            
                    
    def linuxwatcher():
            p = HandleEvents()
            wm = pyinotify.WatchManager()  # Watch Manager
            p.wm = wm
            p.mask = pyinotify.IN_CREATE | pyinotify.IN_DELETE | pyinotify.IN_MODIFY | pyinotify.IN_ATTRIB | pyinotify.IN_MOVED_TO | pyinotify.IN_MOVED_FROM # watched events

            notifier = pyinotify.Notifier(wm, p)
            p.init_watches(op.abspaths, op.follow)
            notifier.loop()

        
#################################################
# END Linux specific code
#################################################

#################################################
# MacOsX specific code
#################################################
if sys.platform == 'darwin':
    from FSEvents import *
    import objc

    def filelevel_approx(path):
            """in order to avoid scanning the entire directory including sub
    directories by unison, we have to say which files have changed. Because
    this is a stupid program it only checks modification times within the 
    update interval. in case there are no files modified in this interval,
    the entire directory is listed.
    A deleted file can not be found like this. Therefore also deletes will 
    trigger a rescan of the directory (including subdirs)

    The impact of rescans could be limited if one could make
    unison work nonrecursively.
    """
            result = []
            #make a list of all files in question (all files in path w/o dirs)
            try:
                    names = os.listdir(path)
            except os.error, msg:
            #path does not exist (anymore?). Add it to the results
                    mydebug("adding nonexisting path %s for sync",path)
                    result.append(path)
                    names = None
                    
            if names:
                    for nm in names:
                            full_path = os.path.join(path,nm)
                            st = os.lstat(full_path)
                            #see if the dir it was modified recently
                            if st.st_mtime>time()-float(op.latency):
                                    result.append(full_path)
            
            if result == []:
                    result.append(path)
            
            return result
            

    def fsevents_callback(streamRef, clientInfo, numEvents, eventPaths, eventMasks, eventIDs):
            mydebug("fsevents_callback(streamRef = %s, clientInfo = %s, numEvents = %s)", streamRef, clientInfo, numEvents)
            mydebug("fsevents_callback: FSEventStreamGetLatestEventId(streamRef) => %s", FSEventStreamGetLatestEventId(streamRef))
            mydebug("fsevents_callback: eventpaths = %s",eventPaths)
            
            full_path = clientInfo

            result = []
            for i in range(numEvents):
                    path = eventPaths[i]
                    if path[-1] == '/':
                            path = path[:-1]

                    if eventMasks[i] & kFSEventStreamEventFlagMustScanSubDirs:
                            recursive = True

                    elif eventMasks[i] & kFSEventStreamEventFlagUserDropped:
                            mymesg("BAD NEWS! We dropped events.")
                            mymesg("Forcing a full rescan.")
                            recursive = 1
                            path = full_path

                    elif eventMasks[i] & kFSEventStreamEventFlagKernelDropped:
                            mymesg("REALLY BAD NEWS! The kernel dropped events.")
                            mymesg("Forcing a full rescan.")
                            recursive = 1
                            path = full_path

                    else:
                            recursive = False

                    #now we should know what to do: build a file directory list
                    #I assume here, that unison takes a flag for recursive scans
                    if recursive:
                            #we have to check all subdirectories
                            if isinstance(path,list):
                                    #we have to check all base paths
                                    allpathsrecursive = [p + '\tr']
                                    result.extend(path)
                            else:
                                    result.append(path+'\tr')
                    else:
                            #just add the path
                            #result.append(path)
                            #try to find out what has changed
                            result.extend(filelevel_approx(path))
                            
            mydebug('Dirs sent: %s',eventPaths)
            #TODO: handle creation/deletion of links that should be followed            
            update_changes(result)

            try:
                    f = open(op.absstatus,'w')
                    f.write('last_item = %d'%eventIDs[-1])
                    f.close()
            except IOError:
                    mymesg('failed to open status file %s', op.absstatus)

    def my_FSEventStreamCreate(paths):
            mydebug('my_FSEventStreamCreate: selected paths are: %s',paths)

            if op.sinceWhen == 'now':
                    op.sinceWhen = kFSEventStreamEventIdSinceNow
                    
            try:
                op.symlinks
            except AttributeError:
                make_symlinks()
                
            for sl in op.symlinks:
                #check if that path is already there
                found=False
                ln = op.symlinks[sl]
                for path in paths:
                    if relpath(op.root,path)==ln:
                        found = True
                        break
                if not found:
                    mydebug('my_FSEventStreamCreate: watch followed link %s',ln)                    
                    paths.append(os.path.join(op.root,ln))
            
            streamRef = FSEventStreamCreate(kCFAllocatorDefault,
                                        fsevents_callback,
                                        paths,				#will this pass properly through? yes it does.
                                        paths,
                                        int(op.sinceWhen),
                                        float(op.latency),
                                        int(op.flags))    
            if streamRef is None:
                    mymesg("ERROR: FSEVentStreamCreate() => NULL")
                    return None

            if op.verbose:
                    FSEventStreamShow(streamRef)
            
            #print ('my_FSE', streamRef)

            return streamRef

    def macosxwatcher():
            #since when? if it is 'now' try to read state
            if op.sinceWhen == 'now':
                    di = conf_parser(op.absstatus)
                    if di and di.has_key('last_item'):
                            #print di['last_item'][-1]
                            op.sinceWhen = di['last_item'][-1]
                            #print op.sinceWhen
            
            streamRef = my_FSEventStreamCreate(op.abspaths)
            #print streamRef
            if streamRef is None:
                    print('failed to get a Stream')
                    exit(1)
                    
            FSEventStreamScheduleWithRunLoop(streamRef, CFRunLoopGetCurrent(), kCFRunLoopDefaultMode)

            startedOK = FSEventStreamStart(streamRef)
            if not startedOK:
                    print("failed to start the FSEventStream")
                    exit(1)

            if op.flush_seconds >= 0:
                    mydebug("CFAbsoluteTimeGetCurrent() => %.3f", CFAbsoluteTimeGetCurrent())

                    timer = CFRunLoopTimerCreate(None,
                    CFAbsoluteTimeGetCurrent() + float(op.flush_seconds), 
                    float(op.flush_seconds),
                    0, 0, timer_callback, streamRef)
                    CFRunLoopAddTimer(CFRunLoopGetCurrent(), timer, kCFRunLoopDefaultMode)

            try:
                    CFRunLoopRun()
            except KeyboardInterrupt:
                    mydebug('stop called via Keyboard, cleaning up.')
            #Stop / Invalidate / Release
            FSEventStreamStop(streamRef)
            FSEventStreamInvalidate(streamRef)
            FSEventStreamRelease(streamRef)
            mydebug('FSEventStream closed')

#################################################
# END MacOsX specific code
#################################################

#################################################
# Windows specific code
#################################################
if sys.platform == 'win32':
	import win32file
	import win32con
	import threading
	
	FILE_LIST_DIRECTORY = 0x0001
	
	def win32watcherThread(abspath,file_lock):
		dirHandle = win32file.CreateFile (
			abspath,
			FILE_LIST_DIRECTORY,
			win32con.FILE_SHARE_READ | win32con.FILE_SHARE_WRITE,
			None,
			win32con.OPEN_EXISTING,
			win32con.FILE_FLAG_BACKUP_SEMANTICS,
			None
		)
		while 1:
			results = win32file.ReadDirectoryChangesW (
				dirHandle,
				1024,
				True,
				win32con.FILE_NOTIFY_CHANGE_FILE_NAME |
					win32con.FILE_NOTIFY_CHANGE_DIR_NAME |
					win32con.FILE_NOTIFY_CHANGE_ATTRIBUTES |
					win32con.FILE_NOTIFY_CHANGE_SIZE |
					win32con.FILE_NOTIFY_CHANGE_LAST_WRITE |
					win32con.FILE_NOTIFY_CHANGE_SECURITY,
				None,
				None
			)
			for action, file in results:
				full_filename = os.path.join (abspath, file)
				# This will return 'dir updated' for every file update within dir, but
				# we don't want to send unison on a full dir sync in this situation.
				if not (os.path.isdir(full_filename) and action == 3):
					file_lock.acquire()
					update_changes_nomangle(full_filename)
					file_lock.release()
	
	def win32watcher():
		file_lock = threading.Lock()
		threads = [ threading.Thread(target=win32watcherThread,args=(abspath,file_lock,)) for abspath in op.abspaths ]
		for thread in threads:
			thread.setDaemon(True)
			thread.start()
			
		try:
			while 1:
				pass
		except KeyboardInterrupt:
			print "Cleaning up."

#################################################
# END Windows specific code
#################################################	

if __name__=='__main__':
	global op

	usage = """usage: %prog [options] root [path] [path]...
This program monitors file system changes on all given (relative to root) paths
and dumps paths (relative to root) files to a file. When launched, this file is 
recreated. While running new events are added. This can be read by UNISON
to trigger a sync on these files. If root is a valid unison profile, we attempt
to read all the settings from there."""
    
	parser = OptionParser(usage=usage)
	parser.add_option("-w", "--sinceWhen", dest="sinceWhen",
                      help="""starting point for filesystem updates to be captured
					  Defaults to 'now' in the first run
					  or the last caputured change""",default = 'now', metavar="SINCEWHEN")
	parser.add_option("-l", "--latency", dest="latency",
                      help="set notification LATENCY in seconds. default 5",default = 5, metavar="LATENCY")
	parser.add_option("-f", "--flags", dest="flags",
                      help="(macosx) set flags (who knows what they mean. defaults to 0",default = 0, metavar="FLAGS")
	parser.add_option("-s", "--flushseconds", dest="flush_seconds",
                      help="(macosx) TIME interval in second until flush is forced. values < 0 turn it off. ",default = 1, metavar="TIME")
	parser.add_option("-o", "--outfile", dest="outfile",
                      help="location of the output file. Defaults to UPATH/changes",default = 'changes', metavar="PATH")
	parser.add_option("-t", "--statefile", dest="statefile",
                      help="(macosx) location of the state file (absolute or relative to UPATH). Defaults to UPATH/state",default = 'state', metavar="PATH")
	parser.add_option("-u", "--unisonconfig", dest="uconfdir",
					help='path to the unison config directory. default ~/.unison',
					default = '~/.unison', metavar = 'UPATH')
	parser.add_option("-z", "--follow", dest="follow",
					help="define a FOLLOW directive. This is equivalent to the -follow option in unison \
					(except that for now only 'Paths' are supported). This option can appear multiple times. \
					if a unison configuration file is loaded, it takes precedence over this option",
					action='append',metavar = 'FOLLOW')
	parser.add_option("-q", "--quiet",
					  action="store_false", dest="verbose", default=True,
					  help="don't print status messages to stdout")
				
	parser.add_option("-d", "--debug",
					  action="store_true", dest="debug", default=False,
					  help="print debug messages to stderr")
	
    
	(op, args) = parser.parse_args()
	
	
	if len(args)<1:
		parser.print_usage()
		sys.exit()
	
	#other paths
	op.absuconfdir = my_abspath(op.uconfdir)
	op.absstatus = os.path.join(op.absuconfdir,op.statefile)
	op.absoutfile = os.path.join(op.absuconfdir,op.outfile)
	
	
	#figure out if the root argument is a valid configuration file name
	p = args[0]
	fn = ''
	if os.path.exists(p) and not os.path.isdir(p):
		fn = p
	elif os.path.exists(os.path.join(op.absuconfdir,p)):
		fn = os.path.join(op.absuconfdir,p)
	op.unison_conf = conf_parser(fn)

	#now check for the relevant information
	root = None
	paths = None
	if op.unison_conf and op.unison_conf.has_key('root'):
		#find the local root
		root = None
		paths = None
		for r in op.unison_conf['root']:
			if r[0]=='/':
				root = r
		if op.unison_conf.has_key('path'):
			paths = op.unison_conf['path']
	if op.unison_conf and op.unison_conf.has_key('follow'):
		op.follow = op.unison_conf['follow']
	else:
		#see if follows were defined
		try:
			op.follow
		except AttributeError:
			op.follow = []
			
	if not root:
		#no root up to here. get it from args
		root = args[0]
		
	if not paths:
		paths = args[1:]
		
	#absolute paths
	op.root = my_abspath(root)
	op.abspaths = [os.path.join(root,path) for path in paths]
	if op.abspaths == []:
		#no paths specified -> make root the path to observe
		op.abspaths = [op.root]
	#print op.root
	#print op.abspaths

	mydebug('options: %s',op)
	mydebug('arguments: %s',args)
	
	#cleaning up the change file
	try:
		f=open(op.absoutfile,'w')
		f.close()
	except IOError:
		mymesg('failed to open output file. STOP.')
		exit(1)

	if sys.platform=='darwin':
		macosxwatcher()
	elif sys.platform.startswith('linux'):
		linuxwatcher()
	elif sys.platform.startswith('win32'):
		win32watcher()
	else:
		mymesg('unsupported platform %s',sys.platform)
	

