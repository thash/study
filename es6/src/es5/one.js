"use strict";

function increment() {
  var n = arguments.length <= 0 || arguments[0] === undefined ? 0 : arguments[0];

  return n + 1;
}
