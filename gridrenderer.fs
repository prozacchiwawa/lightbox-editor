module GridRenderer

open Point
open Grid
open Html

let view html grid =
  let svg = html.svg in
  let linePx x1 y1 x2 y2 =
    svg.line
      [
        {name = "stroke" ; value = "black"};
        {name = "x1" ; value = Util.toString x1} ;
        {name = "y1" ; value = Util.toString y1} ;
        {name = "x2" ; value = Util.toString x2} ;
        {name = "y2" ; value = Util.toString y2}
      ]
  in
  if grid.enabled then
    [
      html.div
        [html.className "grid"]
        []
        [
          svg.root 
            [
              {name = "width" ; value = "1000"} ;
              {name = "height" ; value = "1000"} ;
              {name = "viewBox" ; 
               value = 
                 String.concat 
                   " " 
                   [Util.toString grid.offset.x;
                    Util.toString grid.offset.y;
                    Util.toString (1000. + grid.offset.x);
                    Util.toString (1000. + grid.offset.y)
                   ]
              }
            ]
            [
              svg.defs
                [
                  svg.pattern
                    "pt" 
                    0. 0. grid.interval.x grid.interval.y
                    [
                      linePx 0. 0. grid.interval.x 0. ;
                      linePx 0. 0. 0. grid.interval.y ;
                    ]
                ] ;
              svg.rect
                [
                  {name = "x" ; value = "0"} ;
                  {name = "y" ; value = "0"} ;
                  {name = "width" ; value = "100000"} ;
                  {name = "height" ; value = "100000"} ;
                  {name = "style" ; value = "fill: url(#pt);"}
                ] [] []
            ]
        ]
    ]
  else
    []
