let rowComponent = ReasonReact.statelessComponent("Row");

let _CELLS_PER_ROW = 24;
let _ROWS_PER_GRID = 3;
let _VIEWPORT_WIDTH = 1297;

type cellTypes =
  | Top
  | Middle
  | Bottom;

let mapRowType = x =>
  switch (x) {
  | "0" => Top
  | "1" => Middle
  | "2" => Bottom
  };

let getRowNumber = (~cellNo: int) : int =>
  float_of_int(cellNo / _CELLS_PER_ROW) |> floor |> int_of_float;

let randomiseTile = Random.init(4);

let wrapRowClass = (~cellNo: int, ~lane: string) => {
  let rowNumber = getRowNumber(cellNo);
  let rowType = mapRowType(string_of_int(rowNumber));
  let cellModifier = ref("");

  /* Todo: do this programmatically and stop being lazy */
  if (cellNo === 6) {
    cellModifier := "stairs";
  };

  if (cellNo === 16) {
    cellModifier := "stairs";
  };

  if (lane === "1") {
    if (cellNo === 2) {
      cellModifier := "blue-fountain";
    };
  };

  if (lane === "2") {
    if (cellNo === 18) {
      cellModifier := "blue-fountain";
    };
  };

  if (lane === "3") {
    if (cellNo === 10) {
      cellModifier := "blue-fountain";
    };
  };

  let row =
    switch (rowType) {
    | Top => "top"
    | Middle => "middle"
    | Bottom => "bottom"
    };
  {j|row-$row $cellModifier|j};
};

let make = (~lane, _children) => {
  ...rowComponent,
  render: self => {
    let totalCells = _ROWS_PER_GRID * _CELLS_PER_ROW;
    let cells = Helpers.range(0, totalCells);
    <div className="row">
      <div
        className="spriteGrid"
        style=(
          ReactDOMRe.Style.make(
            ~gridTemplateColumns={j|repeat($_CELLS_PER_ROW, 1fr)|j},
            (),
          )
        )>
        (
          ReasonReact.arrayToElement(
            Array.of_list(
              List.map(
                i =>
                  <div
                    key={j|row-$i|j}
                    className=(Cn.make(["cell", wrapRowClass(i, lane)]))
                  />,
                cells,
              ),
            ),
          )
        )
      </div>
    </div>;
  },
};