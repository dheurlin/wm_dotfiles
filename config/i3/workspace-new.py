#!/usr/bin/env python3

import i3ipc
import re
from argparse import ArgumentParser

i3 = i3ipc.Connection()

def main():
    parser = ArgumentParser(description='''
    Simple script to go to a new workspace. It will switch to a workspace with the lowest available number. If invoked whith the argument "--move", it will move the focused container to the new workspace.
    ''')
    parser.add_argument('--move', help="If set, will move the focused window to a new workspace. --move stay will move the window and stay on the curent workspace, while -move move will also move to the newly created workspace.")
    args = parser.parse_args()

    new = get_new_workspace_id()
    if (args.move):
        i3.command("move container to workspace %s" % new)
        if (args.move == 'move'): i3.command ("workspace %s" % new)
    else:
        i3.command("workspace %s" % new)

def get_new_workspace_id():
    workspaces = i3.get_workspaces()
    numbered_workspaces = filter(lambda w: w.name[0].isdigit(), workspaces)
    numbers = list(map(lambda w: int(re.search(r'^([0-9]+)', w.name).group(0)), numbered_workspaces))

    return min(i for i in range(1,max(numbers) + 2) if i not in numbers)

if __name__ == '__main__':
    main()
