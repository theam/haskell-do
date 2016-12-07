"use strict";

var $ = require("jquery");

function objectify(array){
    var object = {};
    array.forEach(function(element) {
        object[element[0]] = element[1];
    });
    return object;
}

exports.fromTextArea = function (textAreaId) {
    return function (configArray) {
        return function () {
            if (configArray.length > 0) {
                return CodeMirror.fromTextArea(document.getElementById(textAreaId), objectify(configArray));
            } else {
                return CodeMirror.fromTextArea(document.getElementById(textAreaId));
            }
        }
    }
}


exports.fromTextAreaMarkdownEditor = function (textAreaId) {
        return function () {
            var textarea = document.getElementById(textAreaId);
            var outerId = "outer-"+textAreaId;
            var editor = new SimpleMDE({ element : textarea, status : false, autofocus : true, toolbar: false});
            editor.codemirror.on("blur", function () {
                editor.togglePreview();
            });
            $('#'+outerId+"> .CodeMirror").dblclick(function () {
                editor.togglePreview();
            });
            return editor;
        }
}

exports._onChange = function (editor) {
    return function (callback) {
        return function () {
            editor.on('change', function(newStuff) {
                return callback(newStuff.getValue());
            });
        }
    }
}
