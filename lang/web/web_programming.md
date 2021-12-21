HTML5 CSS DOM Javascript
---

Three Conceptual component of a web page:
1. content (HTML)
2. presentation (CSS)
3. behavior (Javascript)

# HTML

skeleton: DOCTYPE, head and body

```html
<!DOCTYPE html>
<html lang="en"> <!-- a hint for some applications rather than hard configuration -->
    <head>
        <meta charset="utf-8"> 
        <meta name="author" content="John Dean">
        <meta name="description" content="Kansas City weather conditions">
        <title>K.C. Weather</title>
        <style>
         h1 {text-align: center;}
         hr {widht: 75%}
        </style>
    </head>

    <body>
        <h1>Kansas City</h1>
        <hr>
        <p>
            It should be pleasant todya with a high of 95 degrees<br>
            With a humidity reading 30%, it should feel like 102 degrees
        </p>
        <div> <!-- Generic container -->
            Tomorrow's temperature: <br>
            high 96, low 65
        </div>
    </body>
</html>
```

HTML5 supports audio and video, drawing two-dimensional shapes and animate them, drag-and-drop functionality

# Coding Conventions

1. Every container element should include an end tag

2. Use lowercase for all tag names and attribute values unless there is a must

3. Surround the value of an attribute with quotes and omit spaces around the equal signs

# Elements

[Content Models](https://html.spec.whatwg.org/#content-models)

Element categories determine the rules of containership.

For each element, HTML5 provides a typical default display properties. By Default, browsers collapse whitespaces (display with only one whitespace character, usually a blank space).

## _block elements_

Non-standard category, an element in the flow category that is not an element in the phrasing category. A block element spans the width of the nonmargin part of its enclosing container, whereas a __phrasing element_'s width matches the width of the element's contents.


## Display Properties

"Typical default display properties" are defined by W3C and recommended for browers.

Avoid using `b`, `u`, `i`.

# CSS

- Cascading: each stage/place has its own set of rules, and each set of rules is referred to as a style sheet. CSS rule application has levels of priority: from local, to head section, to external files, to user settings and of the lowest, the browser's native default settings.

CSS rules are applied to elements within a web page. _Selectors_ are used to determine to which elements CSS rules are applied.

- _type selector_: `hr { width: 50% }`, set the width of every `hr`.

- _universal selector_: `* { text-align: center; }`, every element that has the `text-align` property.

- _class selector_: `.red { background-color: tomato }`, select elemetns that have a particular value for their `class` attribute. Class selectors can be combined with element type or universal selectors: `element-type.class-value { property1: value; property2: value; }`

- _ID selectors_: unique id within a particular web page `#id-value { property1: value; property2: value; }`


```css
selectors(s) { property1: value; property2: value; }
```

CSS can be directly written inside of the `style` attribute of any element (avoid using it to separate presentation from content).

The recommended way is to use external CSS files.

```html
<link ref="stylesheet" href="name-of-external-file"> <!-- in the header -->
```

## Partial Styling

Use `span` (nonblock) or `div`(block element) and specify their classes so that selectors can match them. More local CSS rules take precedence over the more global CSS rule (principle of locality).

## CSS Properties

- Color properties: `color`, `background-color`

- Font propertiese: `font-style` (normal/slant/oblique), `font-variant` (normal/small-caps), `font-weight` (normal/bold/bolder/lighter/...), `font-size` (xx-small/.../large/larger/1em), `font-family` (Courier/Prestige/..., use generic names as fallbacks)
