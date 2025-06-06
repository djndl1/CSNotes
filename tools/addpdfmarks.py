# -*- coding: utf-8 -*-

import json

# [/Page 7 /Count -2 /Title (1. Understanding Global Concepts)  /OUT pdfmark

class PdfMarkItem:
    def __init__(self, page, title, expand_subitems=False):
        self._page = page
        self._title = title
        self._expand_subitems = expand_subitems
        self._subitems = []

    def add_subitem(self, subitem) :
        self._subitems.append(subitem)

    def as_pdfmark_text(self, level=0):
        expanding_token = "" if self._expand_subitems else "-"
        count_text = f"/Count {expanding_token}{len(self._subitems)}" if self._subitems else ""

        indent = '    ' * level
        item_text = f'{indent}[/Page {self._page} {count_text} /Title ({self._title}) /OUT pdfmark'

        subitem_texts = [item.as_pdfmark_text(level+1) for item in self._subitems]
        subitem_text = '\n'.join(subitem_texts)

        if self._subitems:
            return '\n'.join([item_text, subitem_text])
        else:
            return item_text

    def __str__(self):
        return self.as_pdfmark_text()

    def __repr__(self):
        return self.as_pdfmark_text()

class JsonPdfMarkParser:
    def __init__(self, expand_subitems=False):
        self._expan_subitems = expand_subitems

    def _load_mark(self, pdfmark_obj: dict) -> PdfMarkItem:
        title = pdfmark_obj["t"]
        page = pdfmark_obj["p"]
        parent_item = PdfMarkItem(page, title)
        if 's' in pdfmark_obj:
            children = [self._load_mark(subobj) for subobj in pdfmark_obj["s"]]
            for child in children:
                parent_item.add_subitem(child)

        return parent_item

    def feed(self, json_str):
        '''
        '''
        list_of_pdfmarks = json.loads(json_str)
        pdfmarks = []
        for mark in list_of_pdfmarks:
            item = self._load_mark(mark)
            pdfmarks.append(item)

        return pdfmarks



if __name__ == '__main__':
    root = '''[
                {
                    "t": "A",
                    "p": 1,
                    "s": [
                        { "t": "A-1", "p": 2 },
                        { "t": "A-2", "p": 2 }
                    ]
                },
                {
                    "t": "B",
                    "p": 2,
                    "s": [
                        { "t": "A-1", "p": 3 },
                        { "t": "A-2", "p": 4 },
                        {
                            "t": "A-3", "p": 4,
                            "s": [
                                { "t": "A-3-1", "p": 5 },
                                { "t": "A-3-1", "p": 6 }
                            ]
                        }
                    ]
                }
            ]'''
    parser = JsonPdfMarkParser()
    marks = parser.feed(root)

    for m in marks:
        print(m)
