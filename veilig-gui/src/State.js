// module State

"use strict";

exports.makeEditor = function (_id) {
  return function(){
    alert("Hi " + _id);
    CodeMirror.fromTextArea(
      document.getElementById(_id.toString()),
      {mode: "haskell"}
    );
  }();
}
