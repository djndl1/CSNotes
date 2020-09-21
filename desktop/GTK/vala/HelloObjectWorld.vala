class HelloWorld
{
    private string name;

    public HelloWorld(string name)
    {
        this.name = name;
    }

    public void greet()
    {
        stdout.printf("Hello" + this.name + "!\n");
    }

}

    int main(string[] args)
    {
         var person = args[1] ?? "You";
         var hello = new HelloWorld(person);
         hello.greet();
         return 0;
    }
