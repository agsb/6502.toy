#/bin/bash
#
#   agsb@2025
#
#   make a list of pins conections for wire wrap
#
#   premisse:
#   each wire have one name,
#   each pin have one or more wires,
#   each wire connect two or more pins,
#   each unit have two or more pins,

#   format:,
#   unit, pin, wire, obs,
#
#   notes:
#
#   one line for each wire on pin,
#   use obs for datasheet use of pin,
#   use unit, 00, 00, x, y, for size of unit
#
#   use nc for not connected
#   use 0V0, for VSS, gnd
#   use 3V3, for VDD,
#   use 5V5, for VCC,
#
cat $1 | \
sort -t',' -k3,3 -k1,2 | \
sed -e '/^[ ]*$/d' | \
tee $1.pns | \
grep -v '# ' | grep -v ' nc,' | \
cat -n > $1.lst

