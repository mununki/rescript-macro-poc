let forwarded = Main.generatedValue
let fromDeriver = Main.getName({name: "Bob"})
let fromSpice =
  Main.encodeDrink({label: "chai"})->JSON.stringify ++
  ":" ++
  Main.decodeDrink(JSON.Object(dict{"label": JSON.String("oolong")})).label
