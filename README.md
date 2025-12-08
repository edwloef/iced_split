<div align="center">

## `iced_split` - resizeable splits for `iced`

[![iced](https://img.shields.io/badge/0.14-blue?logo=iced&style=for-the-badge)](https://github.com/iced-rs/iced)
[![license](https://img.shields.io/badge/License-MIT-blue?style=for-the-badge)](https://github.com/edwloef/iced_split/blob/master/LICENSE)
[![crates.io](https://img.shields.io/crates/v/iced_split?style=for-the-badge)](https://crates.io/crates/iced_split)
[![docs.rs](https://img.shields.io/docsrs/iced_split?style=for-the-badge)](https://docs.rs/iced_split)

</div>

## Overview

`iced_split` provides a pane-style resizeable split widget for use with the [`iced`](https://github.com/iced-rs/iced) GUI library.

```rust
use iced::Element;
use iced_split::vertical_split;

enum Message {
	OnDrag(f32),
}

struct State {
	split_at: f32,
}

impl State {
	fn update(&mut self, message: Message) {
		match message {
			Message::OnDrag(split_at) => self.split_at = split_at,
		}
	}

	fn view(&self) -> Element<'_, Message> {
		vertical_split(
			"left pane",
			"right pane",
			self.split_at,
			Message::OnDrag,
		)
		.into()
	}
}
```

## Compatibility

Refer to the table below to determine which version(s) of `iced_split` are compatible with your version of `iced`.

| `iced`     | `iced_split` |
|------------|--------------|
| `0.14.0`   | `0.1.0`      |
| `0.15-dev` | `0.2-dev`    |

## License

`iced_split` is licensed under the [MIT License](https://mit-license.org/). By contributing to `iced_split`, you agree that your contributions will be licensed under the MIT as well.
