// Expands to: let generatedValue = "macroValue"
%%foo.a.graphql("macroValue")

// Expands to: let message = "query:" ++ "spice"
let message = %foo.a.graphql("spice")

// Expands to: let getName = value => value.name
@foo.a.accessors
type user = {name: string}

// Expands to functions like:
// let encodeDrink = value => { let object_ = Dict.make(); Dict.set(object_, "label", JSON.Encode.string(value.label)); JSON.Encode.object(object_) }
// let decodeDrink = json => { let object_ = json->JSON.Decode.object->Option.getOrThrow; {label: object_->Dict.getUnsafe("label")->JSON.Decode.string->Option.getOrThrow} }
@foo.a.spice
type drink = {label: string}

let alice: user = {name: "Alice"}
let userName = getName(alice)
let encodedDrink = encodeDrink({label: "chai"})
let decodedDrink = decodeDrink(JSON.Object(dict{"label": JSON.String("oolong")}))
let summary =
  message ++
  ":" ++
  generatedValue ++
  ":" ++
  userName ++
  ":" ++
  JSON.stringify(encodedDrink) ++
  ":" ++
  decodedDrink.label
