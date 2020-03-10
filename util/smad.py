# decode a Prime pdev number on the command line

import sys

if len(sys.argv) < 2:
    raise Exception, 'Usage: smad <Prime physical device number>'

pdev = int(sys.argv[1], 8)
if not (pdev & 0x10):
    raise Exception, 'Not a valid pdev'
cont = (pdev & 0xE0) >> 5

print 'Controller   %d @ \'%d' % (cont, [24, 26, 25, 22, 45, 27, 46, 23][cont])
print 'Unit         %d' % ((pdev & 0xf) >> 1)
print 'Head offset  %d' % ((pdev >> 12) * 2)
print 'Surfaces     %d' % (((pdev >> 7) & 0x1E) + (pdev & 1))
