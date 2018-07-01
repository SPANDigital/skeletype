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

let getRowNumber = (~cellNo: int) : string =>
  float_of_int(cellNo / _CELLS_PER_ROW)
  |> floor
  |> int_of_float
  |> string_of_int;

let randomiseTile = Random.init(4);

let wrapRowClass = (~cellNo: int) => {
  let cellNo:string = getRowNumber(cellNo);
  let rowType = mapRowType(cellNo);
  let row =
    switch (rowType) {
    | Top => "top"
    | Middle => "middle"
    | Bottom => "bottom"
    };
  {j|row-$row|j};
};

let make = (~number, _children) => {
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
                i => <div className=(Cn.make(["cell", wrapRowClass(i)])) />,
                cells,
              ),
            ),
          )
        )
      </div>
    </div>;
  },
};