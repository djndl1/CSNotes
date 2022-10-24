package main

const spanishLang = "Spanish"
const frenchLang = "French"
const englishLang = "English"

const englishHelloPrefix = "Hello, "
const spanishHelloPrefix = "Hola, "
const frenchHelloPrefix = "Salut, "

const englishWorld = "World"
const spanishWorld = "Mundo"
const frenchWorld = "Monde"

func Hello(name string, language string) string {

	if name == "" {
		name = defaultWorld(language)
	}

	return greetingPrefix(language) + name
}

func defaultWorld(language string) (world string) {
	switch language {
	case frenchLang:
		world = frenchWorld
	case spanishLang:
		world = spanishWorld
	case englishLang:
		world = englishWorld
	}

	return
}

func greetingPrefix(language string) (prefix string) {
	switch language {
	case frenchLang:
		prefix = frenchHelloPrefix
	case spanishLang:
		prefix = spanishHelloPrefix
	case englishLang:
		prefix = englishHelloPrefix
	}

	return
}
