#!/usr/bin/env python3

import i3ipc
import re
from itertools import *
from argparse import ArgumentParser

i3 = i3ipc.Connection()

def main():
    parser = ArgumentParser(description='''
    A script to go to the next or previous workspace on the current monitor.
    ''')
    parser.add_argument('--next', help="Move to the next available workspace", action="store_true")
    parser.add_argument('--prev', help="Move to the previous available workspace", action="store_true")
    args = parser.parse_args()

    ws = i3.get_workspaces()

    curr    = next(filter((lambda w: w.focused), ws))
    targets = filter((lambda w: w.output == curr.output), ws)
    target  = curr

    # To get the next workspace
    if args.next:
        targets = dropwhile(lambda w: w.num != curr.num, targets)
        target  = next(targets) # This is going to be the current workspace
        try:  # try to get the next one
            target = next(targets)
            print(target.num)
        except:
            pass

    # To get the previous workspace
    else:
        targets = takewhile(lambda w: w.num != curr.num, targets)
        try:
            *_, target  =  targets # get the last element in the iterator
        except: pass

    i3.command(f'workspace {target.name}')

if __name__ == '__main__':
    main()

