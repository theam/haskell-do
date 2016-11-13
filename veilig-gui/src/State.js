// module State

"use strict";

exports.makeEditor = function (_id) {
  return function(){
    CodeMirror.fromTextArea(
      document.getElementById(_id.toString()),
      {mode: "haskell"}
    );
  }();
}
