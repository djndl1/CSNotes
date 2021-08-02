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
