"use strict";

var $ = require("jquery");
require("jquery-ui");

exports.toggleColumns = function () {
    $('#right-column').toggleClass('col-lg-4 col-lg-push-8');
    $('#right-column').toggle();
    $('#left-column').toggleClass('col-lg-8 col-lg-pull-4 col-lg-12');
}
