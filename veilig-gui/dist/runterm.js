function makeInitialTextReadOnly(input) {
    var readOnlyLength = input.value.length;
    input.addEventListener('keydown', function(event) {
        var which = event.which;
        if (((which == 8) && (input.selectionStart <= readOnlyLength))
                || ((which == 46) && (input.selectionStart < readOnlyLength))) {
            event.preventDefault();
        }
    });
    input.addEventListener('keypress', function(event) {
        var which = event.which;
        if ((event.which != 0) && (input.selectionStart < readOnlyLength)) {
            event.preventDefault();
        }
    });
}

makeInitialTextReadOnly(document.getElementById('console'));
