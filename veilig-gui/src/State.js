// module State

"use strict";

socket.onopen = function () {
    socket.send("Hi! I am client");
}

exports.makeEditor = function (_id) {
  return function(){
    document.getElementById(_id.toString()).value = "";
    CodeMirror.fromTextArea(
      document.getElementById(_id.toString()),
      {mode: "haskell"}
    );
  }();
}

exports.checkNotebookImpl = function (notebook) {
    return function () {
        socket.send(notebook);
    }
}
