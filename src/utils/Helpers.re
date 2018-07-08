let rec range = (start: int, end_: int) =>
  if (start >= end_) {
    [];
  } else {
    [start, ...range(start + 1, end_)];
  };

let filter = (~f, arr) =>
  arr |> ArrayLabels.to_list |> ListLabels.filter(~f) |> ArrayLabels.of_list;

let find = (~f, arr) => arr |> ArrayLabels.to_list |> ListLabels.find(~f);

let rec fill = (~element: 'a, ~length: int) =>
  if (length <= 0) {
    [];
  } else {
    [element, ...fill(~element, ~length=length - 1)];
  };

let contains = (~value: 'a, theArray: array('a)) => {
  let f = (found, elem) => found || elem == value;
  ArrayLabels.fold_left(~f, ~init=false, theArray);
};

let rec getElementAt = (~index: int, l: list('a)) =>
  switch (l) {
  | [] => None
  | [head, ...tail] =>
    if (index <= 0) {
      Some(head);
    } else {
      getElementAt(~index=index - 1, tail);
    }
  };