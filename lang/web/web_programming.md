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
