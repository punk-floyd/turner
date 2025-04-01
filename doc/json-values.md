# JSON Values

In Turner, a JSON value (`json::value`) is represented as a thinly-wrapped
`std::variant`.

| JSON type | Variant type     | Effective C++ type                   |
| --------- | ---------------- | ------------------------------------ |
| object    | `json::object`   | `std::map<std::string, json::value>` |
| array     | `json::array`    | `std::vector<json::value>`           |
| string    | `json::string`   | `std::string`                        |
| number    | `json::number`   | `double`                             |
|           | `json::integer`  | `std::int64_t`                       |
| `"true"`  | `bool`           | `bool`                               |
| `"false"` | `bool`           | `bool`                               |
| `"null"`  | `std::nullptr_t` | `std::nullptr_t`                     |

In addition to the standard facilities for accessing `std::variant` values
such as `std::get` and `std::visit`, Turner adds a few helper routines for
working with the JSON data. For each JSON type above, the following methods
are available:
 - `is_foo` returns true if value holds value of type `foo`
 - `get_foo` returns a reference to the stored variant value or throws
   `std::bad_variant_access` on variant type error.
 - `get_foo_if` returns a pointer to the stored variant value or `nullptr`
   if the variant holds a different type.

## JSON Value Interrogation Examples
```c++
#include <turner/json.h>

using namespace turner;

// Parse some JSON data and access the root JSON value
json data{/*...*/};
const json::value& value = data.get_value();

// Walk all object members using is_foo/get_foo
if (value.is_object()) {
    for (const auto& [member_name, member_val] : value.get_object()) {
        // ...
    }
}

// Walk all array members using get_foo_if
if (const auto* array = value.get_array_if()) {
    for (const auto& element : *array) {
        // ...
    }
}

// Using std::visit
std::visit(turner::imp::Overload {
    [](const json::object&  v) { /* v is a JSON object */ },
    [](const json::array&   v) { /* v is a JSON array */  },
    [](const json::string&  v) { /* v is a JSON string */ },
    [](std::nullptr_t       v) { /* v is a JSON null */ },
    [](json::integer        v) { /* v is an integer */    },
    [](json::number         v) { /* v is a JSON number */ },
    [](bool                 v) { /* v is a JSON true/false */ }
}, value);

```
