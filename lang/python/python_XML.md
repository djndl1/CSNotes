# XML basic background knowledge

Extensible Markup Language (XML) is a markup language that defines a set of rules for encoding documents in a format that is both human-readable and machine-readable. It is a textual data format with strong support via Unicode for different human languages. Although the design of XML focuses on documents, the language is widely used for the representation of arbitrary data structures. Several schema systems exist to aid in the definition of XML-based languages. XML has come into common use for the interchange of data over the Internet.

## Concepts

- Character: an XML document is a string of characters.

- markup and content: inside `<>` or `&;`.

- tag: a markup constuct that begins with `<` and `>`. start-tag (`<section>`), end-tag (`</section>`), empty-element (`<line-break />`)

- element: a logical component that begins with a start-tag and ends with a matching end-tag or consists only of an empty tag.

- attribute: An attribute is a markup construct consisting of a name–value pair that exists within a start-tag or empty-element tag.

- XML declaration: some information about XML documents themselves.

## Characters and Encoding

XML documents consist entirely of characters from Unicode. XML allows the use of any of the Unicode-defined encodings, and any other encodings whose characters also appear in Unicode. XML also provides a mechanism whereby an XML processor can reliably, without any prior knowledge, determine which encoding is being used.

XML provides escape facilities for including characters that are problematic to include directly.

```xml
&lt; <!-- < -->
&amp; <!-- & -->
&#20013; <!-- 中 -->
```

Comments cannot be nested.

## Schemas and validation

Validity of an XML document means that it contains a reference to a Document Type Definition and that its elements and attributes are declared in that DTD and follow the grammatical rules for them that the DTD specifies. A newer schema language, described by the W3C as the successor of DTDs, is XML Schema, often referred to by the initialism for XML Schema instances, XSD (XML Schema Definition). XSDs are far more powerful than DTDs in describing XML languages.

## `xml.etree.ElementTree`

The `xml.etree.ElementTree` module implements a simple and efficient API for parsing and creating XML data.

This module has limited support for XPath expressions as a query language.

# `ElementTree`

This class represents an entire element hierarchy, and adds some extra support for serialization to and from standard XML.

`ElementTree.tostring()` can be used to serialize XML.

### `Element`

Represents a single node in this tree.


## `lxml` - a third party library

100% compatible with `ElementTree`, with full XPath 1.0. For large XML documents, `lxml` is significantly faster than the built-in `ElementTree` library.

## The Document Object Model API (DOM)

The Document Object Model, or “DOM,” is a cross-language API from the World Wide Web Consortium (W3C) for accessing and modifying XML documents. The DOM is a standard tree representation for XML data.

A DOM implementation presents an XML document as a tree structure, or allows client code to build such a structure from scratch. It then gives access to the structure through a set of objects which provided well-known interfaces.

The DOM is extremely useful for random-access applications. Some applications are simply impossible in an event driven model with no access to a tree.

See [DOM Level 2 Core Specification](https://www.w3.org/TR/2000/REC-DOM-Level-2-Core-20001113/Overview.html#contents)

### `xml.dom`


