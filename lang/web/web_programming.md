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

Element categories determine the rules of containership.

- _Meta Category_: All the elements allowed in the `head` container.

- _Flow Category_: plain text and all the elements that are allowed ina web page `body` container.

- _Phrasing Category_: inline

- _Embedded Category_: elements that refer to a resource that's separate from the current web page

- _Interactive Category_: elements that are intended for user interaction

- _Heading Category_: elements that define a header for a group of related content

- _Sectioning Category_: elements that define a group of related content

For each element, HTML5 provides a typical default display properties. By Default, browsers collapse whitespaces (display with only one whitespace character, usually a blank space).

`<pre></pre>` preformatted text element for text that needs to have its whitespace preserved.

`<q></q>`: inline quote

`<cite>`: citation

`dfn`: defining instance: rendered with italics

`abbr`: an abbreviation or acronym: a tooltip might pop up

`time`: indicte that its text represents a date or time

`code`: its enclosed text is programming code

`kbd`: keyborad input for a program

`samp`: output for a program

`var`: a math or programming variable

`<br>`, `<wbr>` (for a long sequence of nonblank characters that forms a pattern or code of some sort)

`sub`, `sup`, `s`, `mark`, `small, 

- strong` (strong importance), `em` (emphatic stress), `b`, `u`, `i`: develpers very often use them only for their appearance (which is not encouraged). The latter three are legacy.

`span`: its presentation characteristics are given to it explicitly by CSS. Use `span` to replace `b`, `u`, `i`

## _block elements_

Non-standard category, an element in the flow category that is not an element in the phrasing category. A block element expands to fill the width of its container. A block element spans the width of the nonmargin part of its enclosing container.

