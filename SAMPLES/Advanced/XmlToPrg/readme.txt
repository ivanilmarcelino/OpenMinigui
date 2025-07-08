How XML extract tags, keys

Below is a prg code generator based on the loaded xml file structure.

In my example, the read paths, keys, values and attributes were used to generate
a DO CASE sequence that I can use in .prg to process the read data from the XML file.

I think that this procedure can be used with any structure of XML files, but you need
to pay attention to use an xml file when creating a DO CASE sequence, in which
all possible keys described in the schema are contained in the structure, including
the optional ones.

If someone would like to get an array with xml data, in the generator below I showed
how to do it using hb_XMLParser.

You can use the generator to create just a Do Case sequence or an almost ready-to-use
.prg code.

In a rather original way, I used resources to self-generate new .prg from source code.

Edward