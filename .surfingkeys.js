// ------------------------------------------------------------------
// mappings
// ------------------------------------------------------------------

//unmap(";j");
//unmap("<Ctrl-6>");
//unmap("B");
//unmap("E");
//unmap("F");
//unmap("R");
//unmap("W");
//unmap("X");
//unmap("cc");
//unmap("cf");
//unmap("g$");
//unmap("g0");
//unmap("ob");
//unmap("og");
//unmap("om");
//unmap("ow");
//unmap("q");
//unmap("sm");
//unmap("t");
//unmap("v");
//unmap("yf");
//unmap("yg");

unmapAllExcept([
  "/",
  "C",
  "D",
  "E",
  "G",
  "N",
  "R",
  "S",
  "b",
  "cs",
  "d",
  "e",
  "f",
  "gU",
  "gg",
  //"h", // yandex music hotkey
  "i",
  "j",
  "k",
  //"l", // yandex music hotkey
  "n",
  "on",
  "r",
  "se",
  "sg",
  //"t", // github hotkey
  "w",
  "x",
  "ya",
  "yt",
  "yv",
  "yy",
]);

// it's not unmapped by unmapAllExcept
unmap("<Ctrl-h>");

//map(",n", "t");
map("<Ctrl-d>", "d");
map("<Ctrl-h>", "E");
map("<Ctrl-l>", "R");
map("<Ctrl-u>", "e");
map("F", "C");
map("H", "S");
map("L", "D");
map("d", "x");
map("gs", "se");
//map("t", "on");
map("yl", "ya");

// ------------------------------------------------------------------
// styling
// ------------------------------------------------------------------

Visual.style('marks', 'background-color: #FFFD38; opacity: 0.4;');
