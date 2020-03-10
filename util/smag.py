# create Prime pdev numbers from prompts

import sys
caddr = [24, 26, 25, 22, 45, 27, 46, 23]

def getn(prompt, valid):
    while 1:
        try:
            n = int(input(prompt + '? '))
        except Exception:
            sys.exit(0)
        if valid(n):
            return n

while 1:
    pdev = 0x10
    h = getn('Head offset (0-30)', lambda h: 0 <= h <= 30 and not h & 1)
    pdev |= h >> 1 << 12
    s = getn('Surfaces (0-31)', lambda s: 0 <= s <= 31)
    pdev |= (s & 1) | ((s & 0x1e) << 7)
    c = getn('Controller address %s' % repr(caddr),
             lambda c: c in caddr)
    pdev |= caddr.index(c) << 5
    u = getn('Unit (0-7)', lambda u: 0 <= u <= 7)
    pdev |= u << 1
    print 'Prime pdev \'%o' % pdev
    print
