# Decoding (Parsing) JSON

```c++
#include <turner/json.h>
#include <iostream>

constexpr auto* json_source = R"|({ "name":"Ford", "towel":true })|";

// Constructors throw std::system_error on decoding errors
try { turner::json decode_in_constructor{json_source}; }
catch (const std::system_error& e) {
    std::cerr << "Failed to parse JSON: " << e.what() << '\n';
}

// The decode methods use return values for decoding errors and do not
// explicitly throw.

// Decode from arbitrary char-based input range or a C string
turner::json data;
const auto res = data.decode(json_source);
if (!res) {
    std::cerr << "Failed to parse JSON: " << res.message() << '\n';
    return -1;
}

// Decode from an input stream: file, standard input, etc.
std::ifstream ifs{"path/to/some/json/file"};
const auto res = data.decode_stream(ifs);
if (!res) { /* 🙁 */ }

// Decode from a file
const auto res = data.decode_file("path/to/some/json/file");
if (!res) { /* 🙁 */ }
```
