package main

import "testing"

func TestHello(t *testing.T) {
	t.Run("Say hello to World", func(t *testing.T) {
		got := Hello("World", "English")
		want := "Hello, World"

		assertCorrectMessage(t, got, want)
	})

	t.Run("Say hello to people", func(t *testing.T) {
		got := Hello("Deng", "English")
		want := "Hello, Deng"

		assertCorrectMessage(t, got, want)
	})

	t.Run("Say hello", func(t *testing.T) {
		got := Hello("", "English")
		want := "Hello, World"

		assertCorrectMessage(t, got, want)
	})

	t.Run("In Spanish", func(t *testing.T) {
		got := Hello("Elodie", "Spanish")
		want := "Hola, Elodie"

		assertCorrectMessage(t, got, want)
	})
}

func assertCorrectMessage(t testing.TB, got, want string) {
	t.Helper()

	if got != want {
		t.Errorf("got %q want %q", got, want)
	}
}
