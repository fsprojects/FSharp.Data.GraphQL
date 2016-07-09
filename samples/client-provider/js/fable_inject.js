"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.graphqlBuildQuery = exports.graphqlLaunchQuery = undefined;

var _fableCore = require("fable-core");

var graphqlLaunchQuery = exports.graphqlLaunchQuery = function ($arg1, $arg2, $arg3, $arg4) {
  return _fableCore.Async.awaitPromise(fetch($arg1, {
    method: 'post',
    body: JSON.stringify({
      query: $arg4
    })
  }).then(function (resp) {
    return resp.json();
  }).then(function (resp) {
    return new Promise(function (resolve, reject) {
      if (resp.errors) {
        reject(errors.join('\n'));
      } else {
        resolve($arg2 ? resp.data[$arg2] : resp.data);
      }
    });
  }));
};

var graphqlBuildQuery = exports.graphqlBuildQuery = function ($arg1, $arg2, $arg3, $arg4) {
  return function () {
    var i = 0,
        openBraces = 0,
        closeBraces = 0;

    while (closeBraces == 0 || closeBraces < 0) {
      switch ($arg2[i]) {
        case "{":
          openBraces++;
          break;

        case "}":
          closeBraces++;
          break;
      }

      i++;
    }

    var queryFields = $arg2.substr(0, i),
        queryFragments = $arg2.substr(i);
    var args = $arg3.map(function (k, i) {
      return k + ": " + JSON.stringify($arg4[i]);
    }).join(", ");
    return "{ " + $arg1 + "(" + args + ") " + queryFields + " }" + queryFragments;
  }();
};