// Generated by BUCKLESCRIPT VERSION 3.1.5, PLEASE EDIT WITH CARE

import * as ListLabels from "bs-platform/lib/es6/listLabels.js";
import * as ArrayLabels from "bs-platform/lib/es6/arrayLabels.js";

function range(start, end_) {
  if (start >= end_) {
    return /* [] */0;
  } else {
    return /* :: */[
            start,
            range(start + 1 | 0, end_)
          ];
  }
}

function filter(f, arr) {
  return ArrayLabels.of_list(ListLabels.filter(f)(ArrayLabels.to_list(arr)));
}

function find(f, arr) {
  return ListLabels.find(f, ArrayLabels.to_list(arr));
}

export {
  range ,
  filter ,
  find ,
  
}
/* No side effect */