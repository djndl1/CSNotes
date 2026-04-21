# Chap.12 Object Recognition

The treatment of digital image processing includes recognition of _individual_ image regions, called _objects_ or _patterns_.

## 12.1 Patterns and Pattern Classes

_Pattern_: arrangement of _descriptors_ (*features*). A _pattern class_ is a family of patterns that share some common properties. Three common pattern arrangements used in practice are _vectors_ (for quantitative descriptions) and _strings_ and _trees_ (for structural descriptions). In some applications, pattern characteristic are best described by structural relationships. _String_ descriptions adequately generate patterns of objects and other entities whose structure is based on relatively simples connectivity of primitives, usually associated with boundary shape. A more powerful approach for many applications is the use of tree descriptions. Basically, most hierarchical ordering schemes lead to tree structures.