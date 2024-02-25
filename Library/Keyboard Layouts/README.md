## Steps to produce Dvorak Pali layout

- Open ABC Extended keyboard layout in Ukelele and save it to .keylayout file
- Run perl script from http://fwvdijk.org to turn ABC layout to Dvorak one
- Open it in Ukelele, hold Option and drag DOT ABOVE, MACRON and FULL STOP keys
  to where they are in original ABC Extended layout (w, a and x keys)
- Make new keys dead keys if they are not yet (dead keys are read in Ukelele).
  It might be necessary to do it one key at a time and then reopen Ukelele

## How to remove standard English layout

- https://lupin3000.github.io/macOS/defaults/
- https://gist.github.com/rizo/7124151
- https://community.jamf.com/t5/jamf-pro/setting-keyboard-layout/m-p/64263/highlight/true

UI doesn't allow to remove English layout (US or Dvorak) if there are no other
standard English layouts left (Dvorak Pali is a custom layout so it's not counted)

So it's necessary to do it manually:

- convert plist to XML
- remove item with standard layout
- convert plist back to binary
- reboot

```
$ plutil -convert xml1 com.apple.HIToolbox.plist
$ plutil -convert binary1 com.apple.HIToolbox.plist
```
