"""

This module contains code involving function calls to external programs.  All
the system-specific information is also localized here.

"""

from __future__ import print_function, division, absolute_import
import sys
import os
import subprocess
import tempfile
import glob
import shutil
import time

##
## Get info about the OS we're running on.
##

import platform

#  Get the version as a tuple of strings: (major, minor, patchlevel)
pythonVersion = platform.python_version_tuple() # sys.version_info works too

# Get the system OS type from platform.system as a string such as "Linux",
# "Windows", or "CYGWIN*".  Note that os.name instead returns something like
# "nt" for Windows and "posix" for Linux and Cygwin.
systemOs = platform.system()
if systemOs[:6].lower() == "cygwin":
    systemOs = "Cygwin"

# Find the number of bits the OS supports.
if sys.maxsize > 2**32: systemBits = 64 # Supposed to work on Macs, too.
else: systemBits = 32


##
## General utility functions for paths and finding the directory path.
##


def getTemporaryFilename(extension="", useProgramTempDir=True):
    """Return the string for a temporary file with the given extension or suffix.  For a
    file extension like .pdf the dot should also be in the passed string.  Caller is
    expected to open and close it as necessary and call os.remove on it after
    finishing with it.  (Note the entire programTempDir will be deleted on cleanup.)"""
    dirName = None # uses the regular system temp dir if None
    if useProgramTempDir: dirName = programTempDirectory
    tmpOutputFile = tempfile.NamedTemporaryFile(
          delete=False, prefix=tempFilePrefix, suffix=extension, dir=dirName, mode="wb")
    tmpOutputFile.close() # this deletes the file, too, but it is empty in this case
    return tmpOutputFile.name


def getTemporaryDirectory():
    """Create a temporary directory and return the name.  The caller is responsible
    for deleting it (e.g., with shutil.rmtree) after using it."""
    return tempfile.mkdtemp(prefix=tempDirPrefix)


def getDirectoryLocation():
    """Find the location of the directory where the module that runs this
    function is located.  An empty directory_locator.py file is assumed to be in
    the same directory as the module.  Note that there are other ways to do
    this, but this way seems reasonably robust and portable.  (As long as
    the directory is a package the import will always look at the current
    directory first.)"""
    import directory_locator as dir_loc
    return getCanonicalAbsolueExpandedDirname(dir_loc.__file__)


def getCanonicalAbsoluteExpandedPath(path):
    """Get the canonical form of the absolute path from a possibly relative path
    (which may have symlinks, etc.)"""
    return os.path.normcase(
               os.path.normpath(
                   os.path.realpath( # remove any symbolic links
                       os.path.abspath( # may not be needed with realpath, to be safe
                           os.path.expanduser(path)))))


def getCanonicalAbsolueExpandedDirname(path):
    """Get the absolute directory name from a possibly relative path."""
    return os.path.dirname(getCanonicalAbsoluteExpandedPath(path))


def samefile(path1, path2):
    """Test if paths refer to the same file or directory."""
    if systemOs == "Linux" or systemOs == "Cygwin":
        return os.path.samefile(path1, path2)
    return (getCanonicalAbsoluteExpandedPath(path1) == 
            getCanonicalAbsoluteExpandedPath(path2))


def getParentDirectory(path):
    """Like os.path.dirname except it returns the absolute name of the parent
    of the dirname directory.  No symbolic link expansion (os.path.realpath)
    or user expansion (os.path.expanduser) is done."""
    if not os.path.isdir(path): path = os.path.dirname(path)
    return os.path.abspath(os.path.join(path, os.path.pardir))


def globIfWindowsOs(path, exactNumArgs=False):
    """Expands any globbing if systemOs is Windows (DOS doesn't do it).  The
    argument exactNumFiles can be set to an integer to check for an exact
    number of matching files.  Returns a list."""
    if systemOs != "Windows": return [path]
    globbed = glob.glob(path)
    if not globbed:
        print("\nWarning in pdfCropMargins: The wildcards in the path\n   "
              + path + "\nfailed to expand.  Treating as literal.",
              file=sys.stderr)
        globbed = [path]
    if exactNumArgs and len(globbed) != exactNumArgs:
        print("\nError in pdfCropMargins: The wildcards in the path\n   "
              + path + "\nexpand to the wrong number of files.",
              file=sys.stderr)
        cleanupAndExit(1)
    return globbed


def convertWindowsPathToCygwin(path):
    """Convert a Windows path to a Cygwin path.  Just handles the basic case."""
    if len(path) > 2 and path[1] == ":" and path[2] == "\\":
        newpath = cygwinFullPathPrefix + "/" + path[0]
        if len(path) > 3: newpath += "/" + path[3:]
        path = newpath
    path = path.replace("\\", "/")
    return path


# Set some additional variables that this module exposes to other modules.
programCodeDirectory = getDirectoryLocation()
projectRootDirectory = getParentDirectory(programCodeDirectory)

# The global directory that all temporary files are written to.  Other modules
# all use the definition from this module.  This makes it easy to clean up all
# the possibly large files, even on KeyboardInterrupt, by just deleting this
# directory.
# programTempDirectory = getTemporaryDirectory()


def removeProgramTempDirectory():
    """Remove the global temp directory and all its contents."""
    if os.path.exists(programTempDirectory):
        maxRetries = 3
        currRetries = 0
        timeBetweenRetries = 1
        while True:
            try:
                shutil.rmtree(programTempDirectory)
                break
            except IOError:
                currRetries += 1
                if currRetries > maxRetries: raise # re-raise the exception
                time.sleep(timeBetweenRetries)
    return


def cleanupAndExit(exitCode):
    """Exit the program, after cleaning up the temporary directory."""
    removeProgramTempDirectory()
    sys.exit(exitCode)
    return


##
## General utility functions for running external processes.
##


def getExternalSubprocessOutput(commandList, printOutput=False, indentString="",
                      splitLines=True, ignoreCalledProcessErrors=False, env=None):
    """Run the command and arguments in the commandList.  Will search the system
    PATH.  Returns the output as a list of lines.   If printOutput is True the
    output is echoed to stdout, indented (or otherwise prefixed) by indentString.
    Waits for command completion.  Called process errors can be set to be
    ignored if necessary."""

    printOutput = False # Useful for debugging to set True.
    
    usePopen = True # Needs to be True to set ignoreCalledProcessErrors True
    if usePopen: # Use lower-level Popen call.
        p = subprocess.Popen(commandList, stdout=subprocess.PIPE,
                             stderr=subprocess.STDOUT, env=env)
        output, errout = p.communicate()
        returncode = p.poll()
        if not ignoreCalledProcessErrors and returncode != 0:
            raise subprocess.CalledProcessError(returncode, commandList, output=output)
    else: # Use a check_output call.
        # Note this does not work correctly if shell=True.
        output = subprocess.check_output(commandList, stderr=subprocess.STDOUT,
                                         shell=False, env=env)

    output = output.decode("utf-8")

    if splitLines or printOutput:
        splitOutput = output.splitlines()
    if splitLines:
        output = splitOutput
    if printOutput:
        print()
        for line in splitOutput:
            print(indentString + line)
        sys.stdout.flush()
    return output


def callExternalSubprocess(commandList,
                           stdinFilename=None, stdoutFilename=None, stderrFilename=None,
                           env=None):
    """Run the command and arguments in the commandList.  Will search the system
    PATH for commands to execute, but no shell is started.  Redirects any selected
    outputs to the given filename.  Waits for command completion."""

    if stdinFilename: stdin = open(stdinFilename, "r")
    else: stdin = None
    if stdoutFilename: stdout = open(stdoutFilename, "w")
    else: stdout = None
    if stderrFilename: stderr = open(stderrFilename, "w")
    else: stderr = None

    subprocess.check_call(commandList, stdin=stdin, stdout=stdout, stderr=stderr,
                          env=env)

    if stdinFilename: stdin.close()
    if stdoutFilename: stdout.close()
    if stderrFilename: stderr.close()

    # The older way to do the above with os.system is below, just for reference.
    # command = " ".join(commandList)
    # if stdinFilename: command += " < " + stdinFilename
    # if stdoutFilename: command += " > " + stdoutFilename
    # if stderrFilename: command += " 2> " + stderrFilename
    # os.system(command)
    return


def runExternalSubprocessInBackground(commandList, env=None):
    """Runs the command and arguments in the list as a background process."""
    if systemOs == "Windows":
        DETACHED_PROCESS = 0x00000008
        p = subprocess.Popen(commandList, shell=False, stdin=None, stdout=None,
                stderr=None, close_fds=True, creationflags=DETACHED_PROCESS, env=env)
    else:
        p = subprocess.Popen(commandList, shell=False, stdin=None, stdout=None,
                stderr=None, close_fds=True, env=env)
    return p # ignore the returned process if not needed


