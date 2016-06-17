#
# This is just a throwaway routine used in testing, to generate random regexes
# to test in the TrieDict (and also in the Python regex, to make sure any
# errors aren't in the patterns themselves).
#

from __future__ import print_function, division, absolute_import
import random
import string
import sys
import time

def genRandomRegex(maxLength, maxNumParts, seedPattern=None, seedExample=None):

    def pattSequence(depth, maxNumParts, inRep=False):
        """Get a regex that is a sequence of regexes pasted together."""
        terminalPartTypes = ["char", "wild"]
        nonTerminalPartTypes = ["rep", "or"]
        partTypes = terminalPartTypes*2 + nonTerminalPartTypes*1 # more terminals
        numParts = rndPattern.randint(1, maxNumParts)
        #print("   "*depth + "Starting pattSequence, the num of parts is:", numParts)
        while True:
            totalParts = 0
            pattPython = ""
            pattTrie = ""
            example = ""
            # Get a random regex for each part and paste them together.
            for part in range(numParts):
                #print("   "*depth + "in while, getting next part")
                pNext = rndPattern.choice(partTypes)
                #print("   "*depth + "choice of type is", pNext)
                if pNext == "char":
                    pp, pt, e = genChars()
                elif pNext == "wild":
                    pp, pt, e = genWild()
                elif pNext == "rep":
                    pp, pt, e = genRep(depth=depth+1)
                elif pNext == "or":
                    pp, pt, e = genOr(depth=depth+1)
                else:
                    print("error"); sys.exit(1)
                #if totalLength > maxLen: continue # ignore part when too large
                totalParts += 1
                pattPython += pp
                pattTrie += pt
                example += e
                #print("   "*depth + "got a part:", pattPython, "   ", pattTrie, "   ", example)
            # We don't want a single outer-level repetition if inRep=True, so redo.
            if inRep and pNext == "rep" and totalParts == 1: continue
            break
        retval = (pattPython, pattTrie, example)
        return retval

    def genChars():
        strlen = rndPattern.randint(1, 3)
        chars = string.letters + string.digits
        charList = [rndPattern.choice(chars) for i in range(strlen)]
        s = "".join(charList)
        return (s, s, s)

    def genWild():
        # TODO no ranges generated
        numChoices = rndPattern.randint(1, 5)
        chars = string.letters + string.digits
        charList = [rndPattern.choice(chars) for i in range(numChoices)]
        s = "".join(charList)
        ex = rndExample.choice(charList)
        retval = ("["+s+"]", "\\["+s+"\\]", ex)
        return retval

    def genRep(depth):
        maxExampleReps = 4
        numLoopReps = rndExample.randint(
            1, maxExampleReps) # need at least one or empty rep group
        rndPattState = rndPattern.getstate()
        exampleSequence = ""
        for i in range(numLoopReps):
            while True:
                pp, pt, e = pattSequence(depth=depth+1,
                                         maxNumParts=max(1, maxNumParts-depth), inRep=True)
                if len(pp) != 0: break # redo if empty, otherwise break
            # Reset pattern state, to get same pattern but new example.
            rndPattern.setstate(rndPattState)
            exampleSequence += e
        # Generate an empty example sequence with equal prob to any non-empty one.
        if rndExample.randint(1, maxExampleReps+1) == 1: exampleSequence = ""
        retval = ("("+pp+")*", "\\*\\("+pt+"\\)", exampleSequence)
        return retval

    def genOr(depth):
        noSingleSectionOr = True
        if noSingleSectionOr: numSections = rndPattern.randint(2, max(2, 5-depth))
        else: numSections = rndPattern.randint(1, max(1, 5-depth))
        ppFinal = ""
        ptFinal = ""
        exList = []
        count = 0
        # Recursively get a regex for each section of the 'or'.
        while True:
            pp, pt, e = pattSequence(depth=depth+1,
                                     maxNumParts=max(1, maxNumParts-depth))
            if len(pp) == 0: continue # redo if empty
            count += 1
            ppFinal += pp + "|"
            ptFinal += pt + "\\|"
            exList.append(e)
            if count == numSections: break
        example = rndExample.choice(exList)
        ppFinal = ppFinal[:-1] # remove final "|"
        ptFinal = ptFinal[:-2] # remove final "\\|"
        retval = ("("+ppFinal+")", "\\("+ptFinal+"\\)", example)
        return retval

    #
    # Body of main function.
    #

    rndPattern = random.Random()
    if seedPattern is not None: rndPattern.seed(seedPattern)
    rndExample = random.Random()
    if seedExample is not None: rndExample.seed(seedExample)
    while True:
        returnTuple = pattSequence(depth=0, maxNumParts=maxNumParts)
        if returnTuple[0]: break # break if not empty string
    return ("^"+returnTuple[0]+"$", returnTuple[1], returnTuple[2])


if __name__ == "__main__":

    seedExample = 34343
    seedExample = 33333
    seedPattern = 454333
    seedPattern = 22119
    #seedPattern = None
    maxNumParts = 3
    for i in range(1000):
        print("="*90)
        pythonPatt, triePatt, example = genRandomRegex(
            maxLength=80, maxNumParts=maxNumParts,
            seedPattern=seedPattern, seedExample=seedExample)
        if seedExample: seedExample += 1
        if seedPattern: seedPattern += 1
        print(pythonPatt, "\n"+example)
