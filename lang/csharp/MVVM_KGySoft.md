# MVVM with KGySoft.CoreLibraries

KGySoft.CoreLibraries provides infrastructure for implementing the MVVM pattern in a technology-agnostic way, supporting both WPF and WinForms applications.

**Resources:**
- [KGySoft.CoreLibraries GitHub](https://github.com/koszeggy/KGySoft.CoreLibraries)
- [KGySoft.ComponentModelDemo](https://github.com/koszeggy/KGySoft.ComponentModelDemo)
- [NuGet: KGySoft.CoreLibraries](https://www.nuget.org/packages/KGySoft.CoreLibraries)

## Overview

KGySoft is **NOT** a full MVVM framework like Prism or CommunityToolkit.Mvvm. Instead, it provides building blocks:

| Component | Namespace | Purpose |
|-----------|-----------|---------|
| `ObservableObjectBase` | KGySoft.ComponentModel | Base for ViewModels with property notification |
| `ModelBase` | KGySoft.ComponentModel | Full-featured: validation + undo/redo + editing |
| `SimpleCommand` / `TargetedCommand` | KGySoft.ComponentModel | ICommand implementations |
| `CommandBindingsCollection` | KGySoft.ComponentModel | Event-to-command binding |
| `ObservableBindingList<T>` | KGySoft.Collections | Observable + BindingList combined |

**Key advantage:** Technology-agnostic - same ViewModels work with WPF and WinForms.

---

## 1. Project Setup

### NuGet Package

```xml
<PackageReference Include="KGySoft.CoreLibraries" Version="*" />
```

KGySoft.CoreLibraries supports:
- .NET Framework 2.0+
- .NET Standard 2.0+
- .NET Core / .NET 5+

### Namespace

```csharp
using KGySoft.ComponentModel;
```

---

## 2. ViewModels with ObservableObjectBase

`ObservableObjectBase` provides `INotifyPropertyChanged` implementation with automatic notification via `Get<T>()`/`Set(value)` methods.

### Basic ViewModel

```csharp
using KGySoft.ComponentModel;

public class MainViewModel : ObservableObjectBase
{
    // Auto PropertyChanged notification
    public string CustomerName
    {
        get => Get<string>();
        set => Set(value);
    }

    public decimal Balance
    {
        get => Get<decimal>();
        set => Set(value);
    }

    // Read-only property with lazy initialization
    public Random RandomInstance => Get(() => new Random());

    // Lazy command creation
    public ICommand SaveCommand => Get(() => new SimpleCommand(OnSave));
    public ICommand ClearCommand => Get(() => new SimpleCommand(OnClear));

    public MainViewModel()
    {
        // Set default value
        Get(() => "Default Name").Invoke(); // Initializes with default
    }

    private void OnSave()
    {
        // Save logic
    }

    private void OnClear()
    {
        CustomerName = null;
        Balance = 0;
    }
}
```

### Property Change Handling

Override `OnPropertyChanged` to react to changes:

```csharp
protected override void OnPropertyChanged(PropertyChangedExtendedEventArgs e)
{
    base.OnPropertyChanged(e);
    
    if (e.PropertyName == nameof(CustomerName))
    {
        // React to CustomerName change
    }
}
```

`PropertyChangedExtendedEventArgs` provides `OldValue` and `NewValue`.

---

## 3. Models with ModelBase

`ModelBase` combines validation, undo/redo, and editing support.

### Class Hierarchy

```
ObservableObjectBase
├── PersistableObjectBase
│   ├── ValidatingObjectBase    ← Validation (IDataErrorInfo)
│   ├── EditableObjectBase      ← Edit sessions (ICanEdit)
│   ├── UndoableObjectBase      ← Undo/redo (ICanUndo)
│   └── ModelBase               ← All features combined
```

### Choosing the Right Base Class

```csharp
// Minimal - just property notification
public class MyViewModel : ObservableObjectBase { }

// With validation
public class MyModel : ValidatingObjectBase { }

// With undo/redo
public class MyModel : UndoableObjectBase { }

// Everything combined
public class MyModel : ModelBase { }
```

### Model Example with Validation

```csharp
using KGySoft.ComponentModel;

public class CustomerModel : ModelBase
{
    public string Name
    {
        get => Get<string>();
        set => Set(value);
    }

    public decimal Balance
    {
        get => Get<decimal>();
        set => Set(value);
    }

    protected override ValidationResultsCollection DoValidation()
    {
        var result = base.DoValidation();
        
        if (string.IsNullOrEmpty(Name))
            result.AddError(nameof(Name), "Name is required");
            
        if (Balance < 0)
            result.AddError(nameof(Balance), "Balance cannot be negative");
        else if (Balance > 10000)
            result.AddWarning(nameof(Balance), "Balance exceeds normal range");
            
        return result;
    }
}
```

### Validation Results

`ValidationResultsCollection` supports:
- `AddError(propertyName, message)` - Error level
- `AddWarning(propertyName, message)` - Warning level
- `AddInformation(propertyName, message)` - Information level

---

## 4. Commands

KGySoft provides technology-agnostic `ICommand` implementations.

### Command Types

KGySoft provides 8 command variants for different scenarios:

| Class | Parameter | Source-Aware | Target |
|-------|-----------|--------------|--------|
| `SimpleCommand` | ❌ | ❌ | ❌ |
| `SimpleCommand<TParam>` | ✅ | ❌ | ❌ |
| `TargetedCommand<TTarget>` | ❌ | ❌ | ✅ |
| `TargetedCommand<TTarget, TParam>` | ✅ | ❌ | ✅ |
| `SourceAwareCommand<TEventArgs>` | ❌ | ✅ | ❌ |
| `SourceAwareCommand<TEventArgs, TParam>` | ✅ | ✅ | ❌ |
| `SourceAwareTargetedCommand<TEventArgs, TTarget>` | ❌ | ✅ | ✅ |
| `SourceAwareTargetedCommand<TEventArgs, TTarget, TParam>` | ✅ | ✅ | ✅ |

**Parameter:** Does the command receive a custom parameter?
**Source-Aware:** Does the command receive source event information?
**Target:** Does the command operate on specific target objects?

### SimpleCommand

```csharp
// No parameter
public ICommand SaveCommand => new SimpleCommand(OnSave);

// With parameter
public ICommand DeleteCommand => new SimpleCommand<Customer>(OnDelete);

private void OnSave()
{
    // Save logic
}

private void OnDelete(Customer customer)
{
    // Delete logic
}
```

### TargetedCommand

Executes once for each target - useful for batch operations:

```csharp
// Static commands (from KGySoft demo)
public static class Commands
{
    public static ICommand Undo { get; } 
        = new TargetedCommand<ICanUndo>(OnUndo);
        
    public static ICommand Redo { get; } 
        = new TargetedCommand<ICanUndoRedo>(OnRedo);

    private static void OnUndo(ICanUndo target)
    {
        if (!target.CanUndo) return;
        target.TryUndo();
    }
}

// Instance command
public ICommand DepositCommand 
    => new TargetedCommand<decimal>(d => Balance += d);

// Usage
DepositCommand.Execute(null, state, 100m); // Adds 100 to Balance
```

### Command State

KGySoft commands are **stateless**. State is managed via `ICommandState`:

```csharp
var command = new SimpleCommand(OnExecute);
var state = new CommandState { Enabled = false };
var adapter = new KGyCommandAdapter(command, state);

// Bind state to UI
state.CreatePropertyBinding(nameof(state.Enabled), nameof(CanExecute), button);
```

### SourceAwareCommand

Use `SourceAwareCommand<TEventArgs>` when you need access to **event data**:

```csharp
// Get mouse position from event arguments
public static ICommand LogMouseCommand => 
    new SourceAwareCommand<MouseEventArgs>(source => 
        Debug.WriteLine($"Mouse: {source.EventArgs.X}, {source.EventArgs.Y}"));

// For state updaters that set Enabled based on clipboard content
public static ICommand SetPasteEnabledCommand => 
    new SourceAwareCommand<ExecuteCommandEventArgs>(sourceData =>
    {
        sourceData.EventArgs.State.Enabled = Clipboard.ContainsText();
    });
```

---

## 4.5. Command Bindings Deep Dive

### CommandBindingsCollection

`CommandBindingsCollection` manages the lifecycle of event-to-command bindings. When disposed, it **releases all event subscriptions** automatically.

```csharp
public partial class MainWindow : Window
{
    private readonly CommandBindingsCollection commandBindings = new();

    public MainWindow()
    {
        InitializeComponent();
        
        // Add binding: command → source event
        commandBindings.Add(MyCommands.DoSomethingCommand, btnDoSomething, nameof(Button.Click));
        
        // With parameter callback
        commandBindings.Add(MyCommands.SetColorCommand)
            .WithParameter(() => GetSelectedColor())
            .AddSource(listBox, nameof(ListBox.SelectionChanged));
    }

    protected override void OnClosed(EventArgs e)
    {
        commandBindings.Dispose(); // Cleans up ALL event subscriptions
        base.OnClosed(e);
    }
}
```

### KGySoft ICommand vs Microsoft ICommand

KGySoft's `ICommand` differs significantly from `System.Windows.Input.ICommand`:

| Aspect | **Microsoft ICommand** | **KGySoft ICommand** |
|--------|----------------------|---------------------|
| State Location | Inside command instance | In `ICommandBinding.State` |
| Access Pattern | Instance members | Static members (shared) |
| Event Binding | Routed events (WPF only) | Any event system |
| CanExecute | Per-command | Per-binding |
| Multiple Sources | Not inherent | Supported |

```csharp
// Microsoft ICommand (stateful, instance-based)
public class SaveCommand : ICommand
{
    public bool CanExecute(object parameter) => CanSave;
    public void Execute(object parameter) { /* ... */ }
    public event EventHandler CanExecuteChanged;
}

// KGySoft ICommand (stateless, binding-centric)
public static partial class MyCommands
{
    // Commands are static - shared across all bindings
    public static ICommand SaveCommand => new SimpleCommand(_ => Save());
}
```

---

### Step-by-Step: What Happens When a Button is Clicked

Understanding how the interfaces work together is key to mastering KGySoft's command binding. Here's the complete flow:

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                           1. VIEW LAYER                                       │
│                                                                              │
│  User clicks a Save button                                                   │
│  └─→ Button raises Click event                                               │
└─────────────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                        2. ICommandBinding                                    │
│                                                                              │
│  The binding (created via AddSource) receives the Click event                │
│                                                                              │
│  binding.Add(MyCommands.SaveCommand, btnSave, nameof(Button.Click));        │
│                    │                    │                │                    │
│                    │                    │                │                    │
│                    │              Source object       Event name             │
│                    │                    │                │                    │
│                    │                    ▼                ▼                    │
│  Creates ICommandSource:                                                   │
│  ┌─────────────────────────────────────────────────────────────┐            │
│  │ CommandSource:                                               │            │
│  │   Source = btnSave           (the control)                 │            │
│  │   Sender = btnSave           (actual sender)               │            │
│  │   TriggeringEvent = "Click"  (event name)                 │            │
│  │   EventArgs = MouseEventArgs (event data)                 │            │
│  └─────────────────────────────────────────────────────────────┘            │
└─────────────────────────────────────────────────────────────────────────────┘
                                    │
                                    │ binding.InvokeCommand(source, eventArgs)
                                    ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                           3. ICommand.Execute()                              │
│                                                                              │
│  The binding calls ICommand.Execute(), passing:                              │
│                                                                              │
│  command.Execute(source,     ← ICommandSource (who triggered)              │
│                   state,      ← ICommandState (Enabled, etc.)               │
│                   target,     ← Target object (for TargetedCommand)          │
│                   parameter); ← Optional parameter                           │
│                                                                              │
│  INSIDE YOUR COMMAND LOGIC:                                                 │
│  ┌─────────────────────────────────────────────────────────────┐            │
│  │ public static ICommand SaveCommand =>                      │            │
│  │     new SimpleCommand(state => {                          │            │
│  │         // state lets you modify Enabled, etc.             │            │
│  │         // But you DON'T have access to source info       │            │
│  │         SaveDocument();                                   │            │
│  │     });                                                   │            │
│  │                                                           │            │
│  │ // OR for source-aware commands:                          │            │
│  │ new SourceAwareCommand<MouseEventArgs>((src, state) => { │            │
│  │     src.EventArgs.X   // Can access event data!          │            │
│  │     src.Source        // Can access the button           │            │
│  │     state.Enabled = false; // Can modify state           │            │
│  │ });                                                       │            │
│  └─────────────────────────────────────────────────────────────┘            │
└─────────────────────────────────────────────────────────────────────────────┘
                                    │
                                    │ If command modifies state (e.g., state.Enabled = false)
                                    ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                        4. ICommandStateUpdater                              │
│                                                                              │
│  When state entries change, updaters are notified                           │
│                                                                              │
│  PropertyCommandStateUpdater.TryUpdateState(source, "Enabled", false)         │
│                                                                              │
│  └─→ For each bound source, it sets the property:                           │
│                                                                              │
│      btnSave.Enabled = false;                                               │
│      menuSave.Enabled = false;  (if same state shared)                      │
└─────────────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                           5. UI UPDATED                                     │
│                                                                              │
│  The buttons are now disabled - all from a single state change!             │
│                                                                              │
│  NO code in the View touched the buttons directly.                          │
│  The command modified state; the updater synced to UI.                      │
└─────────────────────────────────────────────────────────────────────────────┘
```

---

### The Key Insight: Separation of Concerns

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                                                                              │
│   COMMAND (Static Logic)           BINDING (Dynamic Connector)             │
│   ┌─────────────────────┐         ┌─────────────────────┐                │
│   │  "WHAT to do"        │         │  "WHAT connects to WHAT" │             │
│   │                     │         │                     │                 │
│   │  - SaveDocument()   │◄───────│  - Command          │                 │
│   │  - DeleteItem()     │         │  - Sources (events) │                 │
│   │  - Undo()           │         │  - State           │                 │
│   │                     │         │  - Updaters        │                 │
│   │  NO KNOWLEDGE OF:   │         │                     │                 │
│   │  - Which button     │         │  NO KNOWLEDGE OF:   │                 │
│   │  - Which event      │         │  - How to save      │                 │
│   │  - UI framework     │         │  - Business logic   │                 │
│   └─────────────────────┘         └─────────────────────┘                │
│                                                                              │
└─────────────────────────────────────────────────────────────────────────────┘
```

---

### What Each Component Contributes

| Component | Role | Example |
|-----------|------|--------|
| **ICommand** | Static logic (what to do) | `SimpleCommand(_ => Save())` |
| **ICommandSource** | Context (who triggered, what event) | `{Source: btnSave, EventArgs: ...}` |
| **ICommandState** | Mutable state (Enabled, Text, etc.) | `{Enabled: true, Text: "Save"}` |
| **ICommandBinding** | Wires command to sources, holds state | `Add(cmd).AddSource(btn, "Click")` |
| **ICommandStateUpdater** | Applies state to UI | Sets `btn.Enabled = state.Enabled` |

---

### Static vs Instance Commands

The tutorial earlier suggested "use static commands." **This is incomplete guidance.** The reality from the demo:

```csharp
// Model/Commands.cs - STATIC commands (generic, targetable)
public static class Commands
{
    // These are STATIC because they work on ANY ICanUndo implementer
    public static ICommand Undo { get; } = new TargetedCommand<ICanUndo>(OnUndo);
    public static ICommand Redo { get; } = new TargetedCommand<ICanUndoRedo>(OnRedo);
}

// MainViewModel.cs - INSTANCE commands (ViewModel-specific)
public class MainViewModel : ObservableObjectBase
{
    // These are INSTANCE because they access ViewModel members
    public ICommand AddItemCommand => Get(() => new SimpleCommand(OnAddItemCommand));
    public ICommand RemoveItemCommand => Get(() => new SimpleCommand<ITestObject>(OnRemoveItemCommand));
    
    // Commands access:
    // - TestList (instance property)
    // - GetListToAccess() (instance method)
    // - RandomInstance (instance property)
    
    private void OnAddItemCommand()
    {
        IList list = GetListToAccess();  // Instance method!
        // ...
    }
}
```

**Comment from demo code (MainViewModel.cs line 85):**
> "Unlike Model.Commands, these are instance commands, which use the MainModelView members."

---

### When to Use Static vs Instance Commands

| Command Type | Use When | Example |
|-------------|----------|---------|
| **Static** | Command operates on any target implementing an interface | Undo/Redo on any `ICanUndo` |
| **Instance** | Command needs to access ViewModel's specific data | Load data, Delete selected item |

```csharp
// ✅ Static: Works on ANY ICanUndo implementer
public static ICommand Undo => new TargetedCommand<ICanUndo>(t => t.TryUndo());

// ✅ Instance: Needs access to ViewModel's Customers list
public class CustomerViewModel : ObservableObjectBase
{
    public ObservableBindingList<Customer> Customers { get; }
    
    public ICommand LoadCommand => Get(() => new SimpleCommand(async () =>
    {
        var data = await LoadAsync();
        Customers.Clear();  // Accessing instance data!
        foreach (var c in data)
            Customers.Add(c);
    }));
}
```

---

### Loading Data for DataGrid (Your Question)

If your command needs to load data into a bound list, **you need instance commands**:

```csharp
public class OrderViewModel : ObservableObjectBase
{
    public ObservableBindingList<Order> Orders { get; } = new ObservableBindingList<Order>();

    // Instance command - accesses Orders list AND SelectedCustomer
    public ICommand LoadOrdersCommand 
        => Get(() => new SimpleCommand(async () =>
        {
            var data = await OrderService.GetOrdersAsync();
            Orders.Clear();
            foreach (var order in data)
                Orders.Add(order);
        }));

    // Instance command - accesses SelectedCustomer
    public ICommand DeleteOrderCommand 
        => Get(() => new SimpleCommand<Order>(order =>
        {
            if (order != null)
                Orders.Remove(order);
        }));
}
```

**You cannot use static commands here** because:
- Static commands can't access `this.Orders`
- Static commands can't access `this.SelectedCustomer`
- Static commands are shared - instance state would leak between callers

---

### Best Practice Summary

```csharp
// Use STATIC for generic, interface-based operations
public static class CommonCommands
{
    public static ICommand Undo { get; } = new TargetedCommand<ICanUndo>(t => t.TryUndo());
    public static ICommand Redo { get; } = new TargetedCommand<ICanUndoRedo>(t => t.TryRedo());
    public static ICommand BeginEdit { get; } = new TargetedCommand<ICanEdit>(t => t.BeginNewEdit());
}

// Use INSTANCE for ViewModel-specific operations
public class CustomerViewModel : ObservableObjectBase
{
    public ICommand LoadCommand => Get(() => new SimpleCommand(OnLoad));
    public ICommand SaveCommand => Get(() => new SimpleCommand(OnSave));
    
    private async void OnLoad() { /* access this.Customers */ }
    private void OnSave() { /* access this.Customers */ }
}
```

### Binding Commands in WPF and WinForms

The same binding pattern works in both WPF and WinForms:

```csharp
// WPF and WinForms - identical binding code!
private CommandBindingsCollection bindings = new();

void SetupBindings()
{
    // Bind command to button click event
    bindings.Add(MyCommands.SaveCommand, btnSave, nameof(Button.Click));
    
    // Bind with state tracking
    bindings.Add(MyCommands.PasteCommand, btnPaste, nameof(Button.Click), pasteState);
    
    // Source-aware command with event args
    bindings.Add<MouseEventArgs>(MyCommands.LogMouseCommand)
        .AddSource(this, nameof(MouseMove));
}

protected override void Dispose(bool disposing)
{
    if (disposing)
        bindings.Dispose();
}
```

### EventToKGyCommand XAML Extension

For WPF, use the markup extension for declarative bindings:

```xml
<Window xmlns:commands="clr-namespace:KGySoft.ComponentModel;assembly=KGySoft.CoreLibraries">
    
    <!-- Simple event binding -->
    <Button Content="Save" 
            Click="{commands:EventToKGyCommand Command={Binding SaveCommand}}"/>
    
    <!-- With specific event -->
    <ListBox x:Name="MyListBox" 
             MouseDoubleClick="{commands:EventToKGyCommand Command={Binding OpenItemCommand}}"/>
    
    <!-- With targets -->
    <TextBox x:Name="InputText"/>
    <Button Content="Process" 
            Click="{commands:EventToKGyCommand Command={Binding ProcessCommand}, 
                                                Targets={Binding ElementName=InputText}}"/>
</Window>
```

### KGyCommandAdapter

Wraps KGySoft commands as WPF `System.Windows.Input.ICommand` for native WPF commanding:

```csharp
// Create adapter for WPF Command property
SaveCommandAdapter = new KGyCommandAdapter(MyCommands.SaveCommand);

// Use with WPF's native Command property
// Button.Command = SaveCommandAdapter;
```

### Best Practices for Commands

1. **Always dispose CommandBindingsCollection:**
   ```csharp
   protected override void Dispose(bool disposing)
   {
       if (disposing)
           commandBindings?.Dispose();
   }
   ```

2. **Add state updaters for UI synchronization:**
   ```csharp
   // ✅ Good - Enabled state syncs to UI
   bindings.Add(MyCommands.ToggleCommand)
       .AddSource(buttonToggle, nameof(Button.Click))
       .AddStateUpdater(PropertyCommandStateUpdater.Updater);
   
   // ❌ Bad - state changes not reflected
   bindings.Add(MyCommands.ToggleCommand)
       .AddSource(buttonToggle, nameof(Button.Click));
   ```

3. **Specify parameter callbacks before sources:**
   ```csharp
   // ✅ Good - parameter set before sources
   bindings.Add(MyCommands.SetColorCommand)
       .WithParameter(() => GetSelectedColor())
       .AddSource(listBox, nameof(ListBox.SelectionChanged));
   
   // ⚠️ Risky - source could fire before parameter set
   bindings.Add(MyCommands.SetColorCommand)
       .AddSource(listBox, nameof(ListBox.SelectionChanged))
       .WithParameter(() => GetSelectedColor());
   ```

4. **Handle errors gracefully:**
   ```csharp
   var binding = bindings.Add(MyCommands.RiskyCommand, button, nameof(Button.Click));
   binding.Error += (s, e) => 
   {
       Log.Error(e.Exception, "Command execution failed");
       e.Handled = true;
   };
   ```

---

## 5. WPF Integration

### Event-to-Command Binding

KGySoft provides markup extensions for XAML event-to-command binding.

#### EventToKGyCommandExtension

```xml
<Window x:Class="MyApp.MainWindow"
        xmlns:commands="clr-namespace:KGySoft.ComponentModel;assembly=KGySoft.CoreLibraries">
    
    <Button Content="Save" 
            Click="{commands:EventToKGyCommand Command={Binding SaveCommand}}"/>
            
    <Button Content="Delete" 
            Click="{commands:EventToKGyCommand Command={Binding DeleteCommand}}"/>
</Window>
```

#### Code-Behind Command Bindings

For complex scenarios, use `CommandBindingsCollection` in code-behind:

```csharp
using KGySoft.ComponentModel;

public partial class MainWindow : Window
{
    private readonly CommandBindingsCollection commandBindings = new();
    
    public MainWindow()
    {
        InitializeComponent();
        
        // Bind ViewModel.PropertyChanged to a command
        commandBindings.Add<PropertyChangedEventArgs>(OnViewModelChangedCommand)
            .AddSource(DataContext, nameof(((MainViewModel)DataContext).PropertyChanged));
    }

    private ICommand OnViewModelChangedCommand 
        => Get(() => new SimpleCommand<PropertyChangedEventArgs>(OnPropertyChanged));
        
    private void OnPropertyChanged(PropertyChangedEventArgs e)
    {
        // Handle property change
    }

    protected override void OnClosed(EventArgs e)
    {
        commandBindings.Dispose();
        base.OnClosed(e);
    }
}
```

### KGyCommandAdapter

Wraps KGySoft commands as WPF `ICommand`:

```csharp
using KGySoft.ComponentModel;

// Create adapter
var saveCommand = new KGyCommandAdapter(new SimpleCommand(OnSave));

// Bind to Button
Button.DataContext = saveCommand;

// In XAML
<Button Content="Save" Command="{Binding}"/>
```

### Validation Markup Extension

```xml
<Window xmlns:validation="clr-namespace:KGySoft.ComponentModel.Validation;assembly=KGySoft.CoreLibraries">

    <!-- Check for validation errors -->
    <TextBox>
        <TextBox.Style>
            <Style TargetType="TextBox">
                <Style.Triggers>
                    <DataTrigger Value="True">
                        <DataTrigger.Binding>
                            <validation:HasValidationResult 
                                Path="{Binding Customer}" 
                                PropertyName="Name"/>
                        </DataTrigger.Binding>
                        <Setter Property="BorderBrush" Value="Red"/>
                    </DataTrigger>
                </Style.Triggers>
            </Style>
        </TextBox.Style>
    </TextBox>

    <!-- Display validation message -->
    <TextBlock Text="{validation:ValidationResult Error, 
                                    Path={Binding Customer}, 
                                    PropertyName=Name}"
               Foreground="Red"/>
</Window>
```

---

## 6. WinForms Integration

WinForms integration uses the same ViewModels but different binding approach.

### WinForms Demo Architecture (From Demo App)

The demo app shows a **hybrid approach** - using BOTH standard WinForms data binding AND KGySoft command binding:

```
┌─────────────────────────────────────────────────────────────┐
│  MainForm (WinForms)                                        │
│                                                              │
│  ┌─────────────────────────────────────────────────────┐   │
│  │ Standard WinForms Data Binding                       │   │
│  │ tbIntPropList.DataBindings.Add(...)                  │   │
│  │ tbStringPropList.DataBindings.Add(...)               │   │
│  └─────────────────────────────────────────────────────┘   │
│                                                              │
│  ┌─────────────────────────────────────────────────────┐   │
│  │ KGySoft Command Binding                              │   │
│  │ commandBindings.Add(cmd, btn, "Click")...            │   │
│  └─────────────────────────────────────────────────────┘   │
│                                                              │
│  ┌─────────────────────────────────────────────────────┐   │
│  │ KGySoft Property Binding (Two-Way)                  │   │
│  │ commandBindings.AddTwoWayPropertyBinding(...)        │   │
│  └─────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────┘
```

---

### WinformsCommandBindingsCollection

KGySoft provides a specialized collection that **auto-adds state updaters** for WinForms:

```csharp
// From demo: WinformsCommandBindingsCollection.cs
public class WinformsCommandBindingsCollection : CommandBindingsCollection
{
    // Automatically adds PropertyCommandStateUpdater and TooltipUpdater
    public override ICommandBinding Add(ICommand command, 
        IDictionary<string, object> initialState = null, bool disposeCommand = false)
        => base.Add(command, initialState, disposeCommand)
            .AddStateUpdater(PropertyCommandStateUpdater.Updater) // for Enabled, etc.
            .AddStateUpdater(TooltipUpdater.Updater);              // for ToolTipText
}
```

**Usage:**
```csharp
// Use WinformsCommandBindingsCollection instead of basic CommandBindingsCollection
private readonly CommandBindingsCollection commandBindings 
    = new WinformsCommandBindingsCollection();

// Now all bindings automatically sync:
// - ICommandState.Enabled → Control.Enabled
// - ICommandState.ToolTipText → ToolTip.SetToolTip()
```

---

### Step-by-Step: How State Syncs to UI Controls in WinForms

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                        1. CREATING THE BINDING                             │
│                                                                              │
│  WinformsCommandBindingsCollection.Add(command, state)                      │
│                                                                              │
│  └─→ Override adds BOTH updaters automatically:                             │
│                                                                              │
│      PropertyCommandStateUpdater.Updater  (for Enabled, Text, etc.)         │
│      TooltipUpdater.Updater                 (for ToolTipText)                │
└─────────────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                        2. STATE HOLDS UI PROPERTIES                        │
│                                                                              │
│  ICommandState is like a dictionary of UI properties:                       │
│                                                                              │
│  var undoState = new CommandState                                          │
│  {                                                                              │
│      { nameof(ToolStripButton.Image), Images.Undo },     // → btnUndo.Image     │
│      { nameof(ToolStripButton.ToolTipText), "Undo" },    // → tooltip           │
│      { "Enabled", true }                                 // → btnUndo.Enabled   │
│  };                                                                             │
│                                                                              │
│  The state is JUST DATA - it doesn't know about any UI controls.             │
└─────────────────────────────────────────────────────────────────────────────┘
                                    │
                                    │ When code sets: undoState.Enabled = false
                                    ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                        3. UPDATER APPLIES STATE TO UI                       │
│                                                                              │
│  PropertyCommandStateUpdater.TryUpdateState(btnUndo, "Enabled", false)        │
│                                                                              │
│  └─→ Uses reflection to set the property:                                    │
│                                                                              │
│      btnUndo.Enabled = false;                                               │
│                                                                              │
│  TooltipUpdater.TryUpdateState(btnUndo, "ToolTipText", "Undo")               │
│                                                                              │
│  └─→ Finds ToolTip component and calls:                                      │
│                                                                              │
│      toolTip.SetToolTip(btnUndo, "Undo");                                   │
└─────────────────────────────────────────────────────────────────────────────┘
```

---

### The Flow: From DataSource Change to Button Update

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                    DATA SOURCE CHANGES (User edits something)               │
│                                                                              │
│  ICanUndo dataSource = ...                                                 │
│  dataSource.BeginNewEdit();                                                │
└─────────────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                    UI CODE RESPONDS TO CHANGE                              │
│                                                                              │
│  dataSource.PropertyChanged += (s, e) =>                                    │
│  {                                                                              │
│      if (e.PropertyName == nameof(ICanUndo.CanUndo))                       │
│          undoState.Enabled = dataSource.CanUndo;    // ← STATE CHANGED     │
│  };                                                                             │
│                                                                              │
│  // NO CODE TOUCHES THE BUTTON DIRECTLY!                                    │
└─────────────────────────────────────────────────────────────────────────────┘
                                    │
                                    │ undoState.Enabled changed
                                    ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                    PROPERTYCOMMANDSTATEUPDATER APPLIES                       │
│                                                                              │
│  TryUpdateState(btnUndo, "Enabled", false)                                   │
│  └─→ btnUndo.Enabled = false                                                │
│                                                                              │
│  TryUpdateState(btnRedo, "Enabled", false)                                   │
│  └─→ btnRedo.Enabled = false                                                │
│                                                                              │
│  (If same state is shared by multiple buttons)                              │
└─────────────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                    BUTTONS ARE UPDATED                                      │
│                                                                              │
│  btnUndo.Enabled = false  ✓                                                │
│  btnRedo.Enabled = false  ✓                                                 │
│                                                                              │
│  The View never touched the buttons directly.                               │
│  It just modified state; the updater did the rest.                        │
└─────────────────────────────────────────────────────────────────────────────┘
```

---

### Why This Matters

| Traditional WinForms | KGySoft WinForms |
|--------------------|--------------------|
| `btnUndo.Enabled = dataSource.CanUndo;` | `undoState.Enabled = dataSource.CanUndo;` |
| View touches button directly | View modifies state |
| Button must exist at compile time | State is just data |
| Complex enable logic scattered | Logic centralized |

---

### Reusable Controls: EditMenuStrip

The demo shows a reusable ToolStrip with undo/redo commands:

```csharp
public class EditMenuStrip : ToolStrip
{
    private readonly CommandBindingsCollection commandBindings 
        = new WinformsCommandBindingsCollection();

    // State objects - control button appearance and enabled state
    private readonly ICommandState undoState = new CommandState
    {
        { nameof(ToolStripButton.Image), Images.Undo },
        { nameof(ToolStripButton.ToolTipText), "Undo last change" }
    };

    public EditMenuStrip()
    {
        Items.AddRange(new ToolStripItem[] { btnUndo, btnRedo, ... });

        // Bind command to button - state auto-syncs properties
        commandBindings.Add(Model.Commands.Undo, undoState)
            .AddSource(btnUndo, nameof(btnUndo.Click));

        // Button gets Image, ToolTipText, Enabled from state automatically
    }

    // Update state when data source changes
    private void ResetUndoState() 
        => undoState.Enabled = (dataSource as ICanUndo)?.CanUndo == true;
}
```

**Key insight:** The button doesn't have explicit properties set - it gets them from the `ICommandState`!

---

### Shared State for Multiple Buttons

One state object controls multiple buttons:

```csharp
// Shared state - one command, multiple targets
private readonly ICommandState commandsWithCurrentItemState 
    = new CommandState { Enabled = false };

// All three buttons share the same state
commandBindings.Add(viewModel.RemoveItemCommand, commandsWithCurrentItemState)
    .WithParameter(() => listBindingSource.Current)
    .AddStateUpdater(PropertyCommandStateUpdater.Updater)
    .AddSource(btnRemove, nameof(btnRemove.Click));

commandBindings.Add(viewModel.SetItemCommand, commandsWithCurrentItemState)
    .WithParameter(() => listBindingSource.Current)
    .AddStateUpdater(PropertyCommandStateUpdater.Updater)
    .AddSource(btnSetItem, nameof(btnSetItem.Click));

commandBindings.Add(viewModel.SetItemPropertyCommand, commandsWithCurrentItemState)
    .WithParameter(() => listBindingSource.Current)
    .AddStateUpdater(PropertyCommandStateUpdater.Updater)
    .AddSource(btnSetProp, nameof(btnSetProp.Click));

// When state changes, ALL three buttons update
commandsWithCurrentItemState.Enabled = bindingSource.Position >= 0;
```

---

### Property Binding (Two-Way Sync)

KGySoft can sync ViewModel properties to control properties:

```csharp
// Two-way binding: ViewModel.BoolProperty ↔ RadioButton.Checked
commandBindings.AddTwoWayPropertyBinding(
    viewModel, 
    nameof(viewModel.UseList),      // ViewModel property
    rbList,                          // Control
    nameof(RadioButton.Checked));    // Control property

// Radio button changes → ViewModel property updates
// ViewModel property changes → Radio button updates
```

**From demo - all radio buttons:**
```csharp
commandBindings.AddTwoWayPropertyBinding(viewModel, nameof(viewModel.UseList), rbList, nameof(RadioButton.Checked));
commandBindings.AddTwoWayPropertyBinding(viewModel, nameof(viewModel.UseBindingList), rbBindingList, nameof(RadioButton.Checked));
commandBindings.AddTwoWayPropertyBinding(viewModel, nameof(viewModel.UseObservableBindingList), rbObservableBindingList, nameof(RadioButton.Checked));
// ... etc
```

---

### ObservableBindingList for DataGridView

```csharp
public class MainViewModel : ObservableObjectBase
{
    // Combines INotifyCollectionChanged + IBindingList
    public ObservableBindingList<CustomerModel> Customers { get; }
        = new ObservableBindingList<CustomerModel>();
}

// In WinForms Form
dataGridView1.DataSource = viewModel.Customers;
```

**Why ObservableBindingList?**
- Implements `IBindingList` (works with DataGridView)
- Implements `INotifyCollectionChanged` (notifies of adds/removes)
- Supports sorting if wrapped collection supports it

---

### Validation Integration

KGySoft provides an adapter to show validation errors in WinForms ErrorProvider:

```csharp
// ValidationResultToErrorProviderAdapter
// Bridges IValidatingObject → WinForms ErrorProvider
public sealed class ValidationResultToErrorProviderAdapter : Component
{
    public ErrorProvider Provider { get; set; }
    public ValidationSeverity Severity { get; set; } // Error, Warning, Information
    public object DataSource { get; set; }
}
```

**Usage in Form:**
```csharp
// Setup error providers
errorProvider.Icon = Images.Error;
warningProvider.Icon = Images.Warning;
infoProvider.Icon = Images.Information;

// ValidationResultToErrorProviderAdapter handles the rest
// Hooks into BindingManager to track validation results
// Updates ErrorProvider with appropriate messages
```

---

### Standard WinForms Data Binding (Hybrid Approach)

The demo shows using standard WinForms binding alongside KGySoft commands:

```csharp
public MainForm()
{
    // Standard WinForms Data Binding (not command-based)
    tbIntPropList.DataBindings.Add(nameof(TextBox.Text), listBindingSource, nameof(ITestObject.IntProp));
    tbStringPropList.DataBindings.Add(nameof(TextBox.Text), listBindingSource, nameof(ITestObject.StringProp));
    
    // But for complex scenarios, use KGySoft command binding
    commandBindings.Add(viewModel.AddItemCommand, btnAdd, nameof(btnAdd.Click));
    
    // And for radio buttons (which behave strangely with standard binding)
    commandBindings.AddTwoWayPropertyBinding(viewModel, nameof(viewModel.UseList), rbList, nameof(RadioButton.Checked));
}
```

**The pattern:** Use standard WinForms binding for simple data, KGySoft for commands and complex UI.

---

### Command Binding via Code

```csharp
public MainForm()
{
    // Basic command binding
    commandBindings.Add(viewModel.SaveCommand, btnSave, nameof(btnSave.Click));
    
    // With state for enabled/disabled
    var saveState = new CommandState { Enabled = false };
    commandBindings.Add(viewModel.SaveCommand, saveState)
        .AddStateUpdater(PropertyCommandStateUpdater.Updater)
        .AddSource(btnSave, nameof(btnSave.Click));
    
    // With parameter from another control
    commandBindings.Add(viewModel.DeleteCommand, deleteState)
        .WithParameter(() => listBindingSource.Current)
        .AddStateUpdater(PropertyCommandStateUpdater.Updater)
        .AddSource(btnDelete, nameof(btnDelete.Click));
}
```

---

### Custom State Updaters

You can create custom state updaters for specific UI properties:

```csharp
// Example: TooltipUpdater syncs ToolTipText from state to control
public class TooltipUpdater : ICommandStateUpdater
{
    public bool TryUpdateState(object commandSource, string stateName, object value)
    {
        if (stateName != "ToolTipText" || !(value is string text))
            return false;
            
        // Finds ToolTip component on parent form
        ToolTip tooltip = GetToolTip(control);
        tooltip.SetToolTip(control, text);
        return true;
    }
}
```

---

### Cleanup

Always dispose the CommandBindingsCollection:

```csharp
protected override void Dispose(bool disposing)
{
    if (disposing)
    {
        components?.Dispose();
        commandBindings.Dispose(); // Releases all event subscriptions
    }
    base.Dispose(disposing);
}
```

---

### Summary: WinForms Patterns from Demo

| Pattern | Code | Purpose |
|---------|------|---------|
| **Hybrid binding** | Standard + KGySoft | Use best tool for each job |
| **WinformsCommandBindingsCollection** | Auto-adds updaters | Enabled, ToolTipText auto-sync |
| **Shared state** | One ICommandState | Multiple buttons synced together |
| **Property binding** | AddTwoWayPropertyBinding | Two-way sync with controls |
| **Reusable controls** | EditMenuStrip | Self-contained with commands |
| **Validation adapter** | ValidationResultToErrorProviderAdapter | Shows validation errors |

---

## 7. Complete Example

### Model Layer

```csharp
// Models/Customer.cs
using KGySoft.ComponentModel;

public class Customer : ModelBase
{
    public string Name
    {
        get => Get<string>();
        set => Set(value);
    }

    public string Email
    {
        get => Get<string>();
        set => Set(value);
    }

    public decimal Balance
    {
        get => Get<decimal>();
        set => Set(value);
    }

    protected override ValidationResultsCollection DoValidation()
    {
        var result = base.DoValidation();
        
        if (string.IsNullOrWhiteSpace(Name))
            result.AddError(nameof(Name), "Name is required");
            
        if (!string.IsNullOrEmpty(Email) && !Email.Contains("@"))
            result.AddError(nameof(Email), "Invalid email format");
            
        if (Balance < 0)
            result.AddError(nameof(Balance), "Balance cannot be negative");
            
        return result;
    }
}
```

### ViewModel Layer

```csharp
// ViewModels/CustomerListViewModel.cs
using KGySoft.ComponentModel;
using KGySoft.Collections;

public class CustomerListViewModel : ObservableObjectBase
{
    public ObservableBindingList<Customer> Customers { get; }
        = new ObservableBindingList<Customer>();

    public Customer SelectedCustomer
    {
        get => Get<Customer>();
        set => Set(value);
    }

    // Commands
    public ICommand AddCommand => Get(() => new SimpleCommand(OnAdd));
    public ICommand DeleteCommand => Get(() => new SimpleCommand<Customer>(OnDelete));
    public ICommand SaveCommand => Get(() => new SimpleCommand(OnSave));

    public CustomerListViewModel()
    {
        // Seed some data
        Customers.Add(new Customer { Name = "John Doe", Email = "john@example.com", Balance = 100 });
    }

    private void OnAdd()
    {
        var customer = new Customer();
        Customers.Add(customer);
        SelectedCustomer = customer;
    }

    private void OnDelete(Customer customer)
    {
        if (customer == null) return;
        Customers.Remove(customer);
        if (SelectedCustomer == customer)
            SelectedCustomer = Customers.FirstOrDefault();
    }

    private void OnSave()
    {
        // Save all customers
        foreach (var customer in Customers)
        {
            if (customer.HasErrors)
            {
                MessageBox.Show($"Please fix errors in {customer.Name}");
                return;
            }
        }
        // Perform save...
    }
}
```

### WPF View

```xml
<!-- Views/CustomerListView.xaml -->
<Window xmlns:commands="clr-namespace:KGySoft.ComponentModel;assembly=KGySoft.CoreLibraries"
        Title="Customer List">
        
    <Grid>
        <Grid.RowDefinitions>
            <RowDefinition Height="Auto"/>
            <RowDefinition Height="*"/>
            <RowDefinition Height="Auto"/>
        </Grid.RowDefinitions>

        <!-- Toolbar -->
        <StackPanel Orientation="Horizontal">
            <Button Content="Add" 
                    Click="{commands:EventToKGyCommand Command={Binding AddCommand}}"/>
            <Button Content="Delete" 
                    Click="{commands:EventToKGyCommand Command={Binding DeleteCommand}}"
                    CommandParameter="{Binding SelectedCustomer}"/>
            <Button Content="Save" 
                    Click="{commands:EventToKGyCommand Command={Binding SaveCommand}}"/>
        </StackPanel>

        <!-- Customer List -->
        <DataGrid Grid.Row="1" 
                  ItemsSource="{Binding Customers}"
                  SelectedItem="{Binding SelectedCustomer}"
                  AutoGenerateColumns="False">
            <DataGrid.Columns>
                <DataGridTextColumn Header="Name" Binding="{Binding Name}"/>
                <DataGridTextColumn Header="Email" Binding="{Binding Email}"/>
                <DataGridTextColumn Header="Balance" Binding="{Binding Balance, StringFormat=C}"/>
            </DataGrid.Columns>
        </DataGrid>

        <!-- Status -->
        <TextBlock Grid.Row="2" 
                   Text="{Binding Customers.Count, StringFormat='Count: {0}'}"/>
    </Grid>
</Window>
```

### WinForms View

```csharp
// Views/CustomerListView.cs (WinForms)
public class CustomerListView : UserControl
{
    private readonly CustomerListViewModel viewModel;
    private DataGridView dataGridView;
    private Button addButton, deleteButton, saveButton;

    public CustomerListView()
    {
        viewModel = new CustomerListViewModel();
        
        SetupControls();
        BindData();
    }

    private void SetupControls()
    {
        dataGridView = new DataGridView { Dock = DockStyle.Fill };
        addButton = new Button { Text = "Add" };
        deleteButton = new Button { Text = "Delete" };
        saveButton = new Button { Text = "Save" };

        var panel = new FlowLayoutPanel
        {
            Dock = DockStyle.Top,
            FlowDirection = FlowDirection.LeftToRight
        };
        panel.Controls.AddRange(new Control[] { addButton, deleteButton, saveButton });

        Controls.Add(dataGridView);
        Controls.Add(panel);
    }

    private void BindData()
    {
        dataGridView.DataSource = viewModel.Customers;
        
        addButton.Click += (s, e) => viewModel.AddCommand.Execute(null);
        deleteButton.Click += (s, e) => viewModel.DeleteCommand.Execute(viewModel.SelectedCustomer);
        saveButton.Click += (s, e) => viewModel.SaveCommand.Execute(null);
    }
}
```

---

## 8. Best Practices

### 1. Choose the Right Base Class

```csharp
// ViewModel - just property notification needed
public class MyViewModel : ObservableObjectBase { }

// Simple model with validation
public class MyModel : ValidatingObjectBase { }

// Model needing undo/redo
public class MyModel : UndoableObjectBase { }

// Complex model with all features
public class MyModel : ModelBase { }
```

### 2. Use Get/Set for Tracked Properties

```csharp
// ✅ Correct - notifications automatic
public string Name
{
    get => Get<string>();
    set => Set(value);
}

// ❌ Wrong - no notification
public string Name { get; set; }
```

### 3. Lazy Command Creation

```csharp
// ✅ Lazy - created on first access
public ICommand SaveCommand => Get(() => new SimpleCommand(OnSave));

// ⚠️ Eager - created immediately
public ICommand SaveCommand { get; } = new SimpleCommand(OnSave);
```

### 4. Dispose Command Bindings

```csharp
public partial class MainWindow : Window
{
    private readonly CommandBindingsCollection commandBindings = new();

    protected override void OnClosed(EventArgs e)
    {
        commandBindings?.Dispose();
        base.OnClosed(e);
    }
}
```

### 5. Validate Before Save

```csharp
private void OnSave()
{
    foreach (var item in Items)
    {
        if (item.HasErrors)  // IValidatingObject.HasErrors
        {
            // Show error and return
            return;
        }
    }
    // Proceed with save
}
```

### 6. Use Undo/Redo in Commands

```csharp
// Static undo/redo commands work with any ICanUndo implementer
public static class EditCommands
{
    public static ICommand Undo { get; } = new TargetedCommand<ICanUndo>(t => t.TryUndo());
    public static ICommand Redo { get; } = new TargetedCommand<ICanUndoRedo>(t => t.TryRedo());
}

// In ViewModel
public ICommand UndoCommand => Get(() => new SimpleCommand(() => 
    EditCommands.Undo.Execute(UndoableModel)));
```

---

## 9. Comparison with Other Frameworks

| Feature | **KGySoft** | **CommunityToolkit.Mvvm** | **Prism** |
|---------|-------------|---------------------------|-----------|
| **Type** | General library | MVVM framework | App framework |
| **Source Generators** | ❌ | ✅ | ❌ |
| **DI Container** | ❌ | ⚠️ | ✅ |
| **Navigation** | ❌ | ❌ | ✅ |
| **Undo/Redo** | ✅ (built-in) | ❌ | ❌ |
| **Validation** | ✅ | ⚠️ | ✅ |
| **Command Pattern** | Stateless, binding-centric | Stateful, instance-based | DelegateCommand |
| **Cross-Platform** | ✅ (WPF + WinForms) | ❌ (WPF only) | ⚠️ (multi-platform) |
| **Platforms** | All .NET | .NET Std 2.0+ | WPF, MAUI, Uno |
| **Legacy .NET 4.0+** | ✅ (primary) | ⚠️ (.NET Std 2.0) | ✅ |

### Key Architectural Difference: Commands

KGySoft uses a **stateless, binding-centric** command pattern vs Microsoft's **stateful, instance-based**:

```
┌─────────────────────────────────────────────────────────────┐
│                    KGySoft Architecture                      │
│                                                              │
│  ViewModel (Commands)                                       │
│  ┌──────────────────────────────────────────────────────┐  │
│  │  INSTANCE Commands (access ViewModel data):           │  │
│  │  - LoadCustomersCommand                               │  │
│  │  - DeleteCustomerCommand                             │  │
│  │  ────────────────────────────────────────────────    │  │
│  │  STATIC Commands (generic, interface-based):          │  │
│  │  - CommonCommands.Undo (targets ICanUndo)            │  │
│  │  - CommonCommands.Redo (targets ICanUndoRedo)       │  │
│  └──────────────────────────────────────────────────────┘  │
│                            │                                │
│                            ▼                                │
│  CommandBindingsCollection (manages sources & state)       │
│                            │                                │
│              ┌─────────────┼─────────────┐                 │
│              ▼             ▼             ▼                 │
│         Button1         Button2       MenuItem             │
│         (state)         (state)       (state)              │
└─────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────┐
│                 Microsoft/WPF Architecture                    │
│                                                              │
│  ViewModel (Instance Commands)                               │
│  ┌──────────────────────────────────────────────────────┐  │
│  │ SaveCommand = new RelayCommand(...) // per-instance  │  │
│  └──────────────────────────────────────────────────────┘  │
│                            │                                │
│                            ▼                                │
│              One Button ←→ One Command (stateful)          │
└─────────────────────────────────────────────────────────────┘
```

### When to Use KGySoft

✅ **Good for:**
- Cross-platform MVVM (WPF + WinForms sharing ViewModels)
- Legacy .NET Framework 4.x applications
- Complex business objects with validation and undo/redo
- Applications needing both `INotifyPropertyChanged` and `IBindingList`

❌ **Consider alternatives:**
- Modern .NET 6+ with source generators → CommunityToolkit.Mvvm
- Enterprise apps with navigation/modules → Prism
- Reactive patterns → ReactiveUI

---

## 10. Command State: When to Use

### The Direct Approach (Simple and Works Fine)

For most applications, you can just set UI controls directly in the View:

```csharp
// In View (ViewModel knows nothing about UI controls)
private void OnViewModelPropertyChanged(object sender, PropertyChangedEventArgs e)
{
    if (e.PropertyName == nameof(ViewModel.CanSave))
        btnSave.Enabled = ViewModel.CanSave;  // Direct, simple, works!
}
```

**This is NOT a violation of MVVM.** The View knows about controls (it's technology-specific anyway). This pattern is simple and readable.

---

### When Command State Actually Helps

Command state is valuable for **one specific scenario**: **one command bound to multiple UI targets**.

```csharp
// You have ONE command:
// - Toolbar button
// - Menu item
// - Context menu
// - Keyboard shortcut indicator

// Without state: Update each target manually
toolbarBtn.Enabled = canSave;
menuItem.Enabled = canSave;
contextMenuItem.Enabled = canSave;

// With state: Set once, all targets update automatically
pasteState.Enabled = canSave;  // All three UI elements synced!
```

---

### Real Demo Example: Multi-Target Binding

From `EditMenuStrip.cs` (WinForms):

```csharp
// View creates state with UI properties
private readonly ICommandState undoState = new CommandState
{
    { nameof(ToolStripButton.Image), Images.Undo },
    { nameof(ToolStripButton.ToolTipText), "Undo last change" }
};

// View sets state based on model capability
private void ResetUndoState() 
    => undoState.Enabled = (dataSource as ICanUndo)?.CanUndo == true;

// One command, ONE binding, ONE state -> button automatically synced
commandBindings.Add(Model.Commands.Undo, undoState)
    .AddSource(btnUndo, nameof(btnUndo.Click));

// When undoState.Enabled changes:
// - PropertyCommandStateUpdater automatically sets btnUndo.Enabled
// - No code touches btnUndo directly
```

---

### Command State Architecture (From Demo)

```
┌─────────────────────────────────────────────────────────────┐
│  VIEW (Creates and owns CommandState)                       │
│                                                              │
│  // View creates state                                       │
│  var undoState = new CommandState { Enabled = false };      │
│                                                              │
│  // View sets state based on business logic                 │
│  undoState.Enabled = model.CanUndo;                         │
│                                                              │
│  // View wires binding - updater handles sync               │
│  bindings.Add(Commands.Undo, undoState)...                  │
└─────────────────────────────────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────┐
│  PropertyCommandStateUpdater                                  │
│                                                              │
│  "state.Enabled changed! Let me update btnUndo for you"     │
└─────────────────────────────────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────┐
│  UI Control (Updated by updater, not by View code)         │
│                                                              │
│  btnUndo.Enabled = true;  // Set automatically!             │
└─────────────────────────────────────────────────────────────┘
```

---

### When NOT to Use Command State

| Scenario | Use Instead |
|----------|------------|
| One command → one button | Direct `btn.Enabled = canSave` |
| Simple enable/disable | Regular PropertyChanged handler |
| No multiple targets | Traditional MVVM pattern |

---

### Honest Assessment

**KGySoft's command state is useful when:**
- ✅ One command controls multiple UI targets simultaneously
- ✅ Building reusable control libraries
- ✅ Need WPF + WinForms to share command logic

**It's overkill when:**
- ❌ One command = one button
- ❌ Simple ViewModel → View communication
- ❌ Just enabling/disabling based on validation

**The "remote control through state" pattern adds indirection without benefit in simple scenarios.**

---

### Best Practice: Start Simple

```csharp
// Start with this (simple, works fine):
private void OnCanSaveChanged(bool canSave)
{
    btnSave.Enabled = canSave;
}

// Only upgrade to CommandState if you actually need:
// - One command → multiple targets
// - Cross-platform command sharing
var pasteState = new CommandState { Enabled = false };
bindings.Add(Commands.Paste, pasteState)
    .AddSource(btnPaste, nameof(Button.Click))
    .AddSource(menuPaste, nameof(MenuItem.Click));
```

---

## 11. Dependencies and Packages

| Package | Purpose |
|---------|---------|
| `KGySoft.CoreLibraries` | Core functionality (required) |
| `KGySoft.CoreLibraries.WinForms` | WinForms-specific extensions |

```xml
<!-- WPF Project -->
<PackageReference Include="KGySoft.CoreLibraries" Version="*" />

<!-- WinForms Project -->
<PackageReference Include="KGySoft.CoreLibraries" Version="*" />
<PackageReference Include="KGySoft.CoreLibraries.WinForms" Version="*" />
```

---

## 12. MVVM vs Event-Driven Programming

### The Core Misconception

MVVM does **NOT** eliminate events. At the UI level, both approaches are event-driven:

```
EVENT-DRIVEN:
User clicks → Click event → btnSave_Click() → SaveData()

MVVM (with command binding):
User clicks → Click event → EventToCommand → SaveCommand.Execute() → SaveData()
```

**The UI layer still responds to events.** The difference is *where business logic lives* and *how UI updates happen*.

---

### Event-Driven WinForms (Traditional)

```csharp
private void btnSave_Click(object sender, EventArgs e)
{
    // Business logic MIXED with UI code
    if (!Validate()) return;
    
    _customer.Name = txtName.Text;
    _customer.Balance = decimal.Parse(txtBalance.Text);
    SaveCustomer(_customer);
    
    // UI updates MIXED with business logic
    btnSave.Enabled = false;
    lblStatus.Text = "Saved!";
}
```

**Problems:**
- Logic tied to button click
- Hard to test without UI
- Logic scattered across event handlers
- UI updates require manual code

---

### MVVM with KGySoft

```csharp
// ViewModel: Business logic SEPARATED
public ICommand SaveCommand => Get(() => new SimpleCommand(() =>
{
    if (!Validate()) return;
    
    Customer.Name = EditName;
    Customer.Balance = EditBalance;
    SaveCustomer(Customer);
    
    StatusMessage = "Saved!";
}));

// View: Wires event to command
commandBindings.Add(vm.SaveCommand, btnSave, nameof(btnSave.Click));

// View: UI updates via BINDING (not manual code)
// <TextBox Text="{Binding EditName}"/>
// <Button Command="{Binding SaveCommand}"/>
// <Label Content="{Binding StatusMessage}"/>
```

---

### What MVVM Actually Changes

| What Changes | Event-Driven | MVVM |
|-------------|--------------|------|
| Business logic location | In event handler | In command |
| UI updates | Manual (`lbl.Text = x`) | Via data binding |
| Enable/disable | Manual (`btn.Enabled = x`) | Via command state or binding |
| Testability | Hard (takes UI) | Easier (commands are plain classes) |

| What Stays the Same | |
|---------------------|---|
| Event still fires | Click → event → binding → command |
| View knows about controls | It's technology-specific anyway |
| Some code in View | Wiring bindings, view-specific concerns |

---

### The Real MVVM WinForms Pattern (From Demo)

The demo (`MainForm.cs`) uses a **hybrid approach**:

```csharp
// Uses STANDARD WinForms data binding (not command-based)
tbIntPropList.DataBindings.Add(nameof(TextBox.Text), listBindingSource, nameof(ITestObject.IntProp));

// Uses KGySoft command binding for events
commandBindings.Add(viewModel.AddItemCommand, btnAdd, nameof(btnAdd.Click));

// Form STILL has event handlers (not pure MVVM!)
private void OnListBindingSourceCurrentItemChangedCommand(ICommandSource src)
{
    // Form manages command state directly
    commandsWithCurrentItemState.Enabled = bindingSource.Position >= 0;
}
```

**Demo comment (line 179-180):**
> "Note that unlike in Model and ViewModel there are no explicit ICommand definitions for these handlers."

---

### Honest Assessment

**MVVM doesn't eliminate events.** It adds a layer that:

1. **Separates business logic** into commands (testable)
2. **Uses data binding** for UI updates (automatic)
3. **Makes testing easier** (commands are plain C# classes)

**Event-driven vs MVVM difference:**
- Event-driven: "Handle event → do everything in handler"
- MVVM: "Handle event → call command → command does logic → binding updates UI"

---

### When MVVM Helps in WinForms

| Scenario | MVVM Benefit |
|----------|--------------|
| Complex forms with many UI updates | Data binding reduces manual updates |
| Business logic complexity | Commands testable without UI |
| Multiple views sharing logic | ViewModels reusable |
| One command → multiple targets | Command state syncs automatically |

| Scenario | MVVM Overkill |
|----------|---------------|
| Simple dialogs | Event handler simpler |
| CRUD forms with few controls | More infrastructure than needed |
| Team unfamiliar with MVVM | Learning curve |

---

### Bottom Line

> **MVVM is still fundamentally event-driven at the UI level.**
>
> The key differences are architectural: where logic lives and how UI updates happen.
>
> For WinForms specifically, MVVM is most valuable when you need complex UI synchronization or plan to migrate to WPF later. For simple forms, event-driven code is often simpler and perfectly acceptable.

---

## 13. MVP vs MVVM

### What is MVP?

**Model-View-Presenter** (MVP) is an older pattern that predates MVVM. It emerged at IBM and Taligent in the 1990s as an evolution of MVC.

**Core idea:** Separate the View from the Model via a Presenter, but the Presenter directly tells the View what to display.

### MVP Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                         VIEW                                 │
│                                                              │
│  - Passive (just widgets, no behavior)                      │
│  - Implements IView interface                               │
│  - No business logic                                        │
│  - Called by Presenter, not calling it directly             │
└─────────────────────────────────────────────────────────────┘
                            ▲
                            │ implements
                            │
┌─────────────────────────────────────────────────────────────┐
│                      IView Interface                          │
│                                                              │
│  Properties: CustomerName, CompanyName, ContactName...     │
│  Methods: DisplayCustomerDetails(), LoadCustomers()         │
└─────────────────────────────────────────────────────────────┘
                            ▲
                            │ receives calls
                            │
┌─────────────────────────────────────────────────────────────┐
│                       PRESENTER                              │
│                                                              │
│  - Contains ALL presentation logic                          │
│  - Has NO knowledge of actual UI technology                 │
│  - Works through IView interface                            │
│  - Testable without UI                                      │
└─────────────────────────────────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────┐
│                        MODEL                                  │
│                                                              │
│  - Domain objects (Customer, Order, etc.)                   │
│  - Service layer                                             │
│  - Complete ignorance of UI                                  │
└─────────────────────────────────────────────────────────────┘
```

### MVP Code Example

```csharp
// IView Interface - View implements this
public interface IViewCustomerView
{
    string CompanyName { set; }
    string ContactName { set; }
    ILookupList CustomerList { get; }
    void DisplayCustomerDetails();
}

// Presenter - tells View what to display
public class ViewCustomerPresenter
{
    private readonly IViewCustomerView view;
    private readonly ICustomerTask task;
    
    public ViewCustomerPresenter(IViewCustomerView view, ICustomerTask task)
    {
        this.view = view;
        this.task = task;
    }
    
    public void Initialize()
    {
        // Presenter directly sets View properties - NO binding
        var customers = task.GetCustomerList();
        customers.BindTo(view.CustomerList);
    }
    
    public void DisplayCustomerDetails()
    {
        int? customerId = GetSelectedCustomerId();
        if (customerId.HasValue)
        {
            CustomerDTO customer = task.GetDetailsForCustomer(customerId.Value);
            
            // Presenter directly pushes data to View
            view.CompanyName = customer.CompanyName;    // Direct assignment!
            view.ContactName = customer.ContactName;    // Direct assignment!
        }
    }
}

// View (Web Form) - implements IView, completely passive
public partial class ViewCustomers : Page, IViewCustomerView
{
    private ViewCustomerPresenter presenter;
    
    protected override void OnInit(EventArgs e)
    {
        base.OnInit(e);
        presenter = new ViewCustomerPresenter(this);
    }
    
    // View just stores what Presenter gives - no binding needed
    public string CompanyName
    {
        set { this.companyNameLabel.InnerText = value; }  // Just passes through
    }
}
```

---

### MVP vs MVVM: Key Differences

| Aspect | MVP | MVVM |
|--------|-----|------|
| **Data flow** | Presenter calls View methods directly | Binding engine syncs automatically |
| **View update** | Manual: `view.Property = value` | Automatic: binding engine |
| **Data holder** | **View holds data** in controls after Presenter pushes | **ViewModel holds data** |
| **Code in Presenter** | Explicit: `view.Name = x` | Not needed |
| **View complexity** | Simpler XAML | More complex XAML/bindings |
| **UI framework dependency** | None - works with any UI | Needs binding engine |

---

### Where Data Lives: The Key Distinction

**MVP: Data Lives in View (UI Controls)**

```
1. Presenter loads from DB:
   CustomerDTO customer = service.GetCustomer(id);

2. Presenter pushes to View:
   view.CompanyName = customer.CompanyName;  // Sets TextBox.Text

3. NOW: Data is in VIEW's controls:
   ┌─────────────────────────────────┐
   │ TextBox.Text = "Acme Inc."     │  ← Data is HERE in View
   │ Label.Text = "John"            │  ← And HERE in View
   └─────────────────────────────────┘
```

**MVVM: Data Lives in ViewModel (Fields)**

```
1. ViewModel loads from DB:
   CustomerDTO customer = service.GetCustomer(id);

2. ViewModel stores in field:
   this.CompanyName = customer.CompanyName;  // Stored in ViewModel

3. Data stays in VIEWMODEL:
   ┌─────────────────────────────────┐
   │ class CustomerViewModel {       │
   │   private string companyName;   │  ← Data is HERE in ViewModel
   │   public string CompanyName {   │    NOT in View
   │     get => companyName;         │
   │   }                            │
   │ }                              │
   └─────────────────────────────────┘
   
4. View just displays via binding:
   <TextBlock Text="{Binding CompanyName}"/>  // Binding engine pulls
```

---

### Visual Comparison

```
MVP (Passive View):
┌──────────────┐     pushes     ┌──────────────┐
│  Presenter   │───────────────►│    View      │
│              │               │ TextBox.Text  │ ← Data HERE
│ service.Get()│               │ Label.Text    │ ← And HERE
└──────────────┘               └──────────────┘


MVP (Supervising Controller):
┌──────────────┐              ┌──────────────┐
│  Model       │─────────────►│    View      │
│  Data        │              │ Binds        │
│              │              │ to Model     │
└──────────────┘              └──────────────┘
         ▲
         │ (logic)
         │
    ┌──────────────┐
    │  Presenter   │
    │  (complex    │
    │   logic)      │
    └──────────────┘


MVVM:
┌──────────────┐              ┌──────────────┐
│  ViewModel   │◄─────────────│    View      │
│              │  (binding)   │              │
│ companyName  │              │ TextBlock    │ ← Just displays
│ (field)      │              │              │   No data stored
└──────────────┘              └──────────────┘
```

---

### MVP Variants

| Variant | Description |
|---------|-------------|
| **Passive View** | View has no data, Presenter pushes everything |
| **Supervising Controller** | View binds to Model directly, Presenter handles complex logic only |

---

### When to Use MVP vs MVVM

| Pattern | Best For | Example |
|---------|----------|---------|
| **MVP** | When you need maximum testability and View is completely passive | Enterprise apps with complex UI logic |
| **MVVM** | When data binding handles most UI updates automatically | WPF/XAML apps with rich binding |
| **Event-Driven** | Simple forms, rapid development | CRUD dialogs |

---

### MVP Advantages

- **Works with ANY UI** - WinForms, Web Forms, WPF, even console
- **Explicit** - Easy to trace, see exactly what happens
- **Presenter controls everything** - No "magic" binding behavior
- **Simple binding** - Just implement interface, Presenter calls setters

### MVVM Advantages

- **Less boilerplate** - No `view.Property = x` everywhere
- **More decoupled** - ViewModel doesn't know about View
- **Better for WPF** - XAML binding engine is rich and powerful
- **Automatic UI updates** - Binding engine handles synchronization

---

### Bottom Line

> **MVP:** Presenter pushes data to View → View holds data in controls
>
> **MVVM:** ViewModel holds data → Binding engine pulls data to View

**MVP and MVVM both separate business logic from UI, but differ in how data flows:**

- **MVP:** `Presenter → View` (push model) - View ends up holding data
- **MVVM:** `ViewModel → Binding → View` (pull model) - ViewModel holds data

**MVP does NOT require data binding.** The Presenter explicitly calls `view.Property = value`. 

**MVVM REQUIRES data binding.** The binding engine automatically pulls data from ViewModel to View.

---

## Summary

KGySoft.CoreLibraries provides **building blocks** for MVVM, but it's not a full MVVM framework.

### What KGySoft Provides

| Component | Purpose | Use Case |
|-----------|---------|----------|
| `ObservableObjectBase` | Property notification | ViewModels with automatic `PropertyChanged` |
| `ModelBase` | Validation + undo/redo + editing | Rich business objects |
| `SimpleCommand` / `TargetedCommand` | Stateless commands | Reusable command logic |
| `CommandBindingsCollection` | Event binding lifecycle | Centralized cleanup |
| `ObservableBindingList` | Observable + BindingList | DataGrid binding |

### Command State: Use Wisely

**Main benefit:** One command → multiple UI targets with synchronized state.

```csharp
// Useful: One state, multiple targets
var pasteState = new CommandState { Enabled = false };
bindings.Add(PasteCommand, pasteState)
    .AddSource(btnPaste, ...)
    .AddSource(menuPaste, ...);  // Both get synced!

// Overkill: One command, one button
bindings.Add(SaveCommand, btnSave, nameof(Button.Click));
// Just use: btnSave.Enabled = canSave;
```

### When to Use KGySoft

| Use Case | Recommendation |
|----------|----------------|
| Cross-platform (WPF + WinForms) sharing commands | ✅ Good fit |
| Legacy .NET 4.x with modern features | ✅ Good fit |
| Complex models with validation/undo | ✅ Good fit |
| Simple one-command-one-button apps | ❌ Use direct `btn.Enabled = x` |
| Modern .NET 6+ apps | ❌ Use CommunityToolkit.Mvvm |

### Bottom Line

KGySoft's command infrastructure is a **sophisticated pattern for sophisticated needs**. For most applications, direct UI property setting in the View is simpler and perfectly acceptable.

**Start simple. Add complexity only when needed.**
