let rec range = (start: int, end_: int) =>
  if (start >= end_) {
    [];
  } else {
    [start, ...range(start + 1, end_)];
  };

let filter = (~f, arr) =>
  arr |> ArrayLabels.to_list |> ListLabels.filter(~f) |> ArrayLabels.of_list;

let find = (~f, arr) => arr |> ArrayLabels.to_list |> ListLabels.find(~f);