#!/usr/bin/python

import re
import subprocess


def get_keychain_pass(account=None, server=None):
    params = {
        'security': '/usr/bin/security',
        'command': 'find-internet-password',
        'account': account,
        'server': server,
        'keychain': '/Users/paul/Library/Keychains/login.keychain',
    }
    command = "sudo -u paul %(security)s -v %(command)s -g -a %(account)s -s %(server)s %(keychain)s" % params
    output = subprocess.check_output(command, shell=True, stderr=subprocess.STDOUT)
    outtext = [l for l in output.splitlines()
               if l.startswith('password: ')][0]

    return re.match(r'password: "(.*)"', outtext).group(1)

def get_keychain_pass_linux(account):
    params = { 'account': account }
    command = "gpg -d ~/%(account)s.gpg" % params
    output = subprocess.check_output(command, shell=True, stderr=subprocess.STDOUT)
    return output.splitlines()[2]
