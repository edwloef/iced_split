#![doc = include_str!("../README.md")]

use iced_core::{
    Animation, Color, Event, Layout, Length, Pixels, Point, Rectangle, Shell, Size, Vector, Widget,
    border::{self, Radius},
    layout::Limits,
    length::{Bounds, Constraint},
    mouse::{self, Click, Cursor, Interaction, click::Kind},
    overlay,
    renderer::{self, Quad},
    time::{Duration, Instant},
    widget::{Meta, Operation, Tree, tree},
    window,
};

/// Creates a new [`horizontal`](Direction::Horizontal) [`Split`] with the given `top` and `bottom`
/// widgets, a split position, and a function to emit messages when the split gets dragged.
pub fn horizontal_split<'a, Message, Top, Bottom, Theme>(
    top: Top,
    bottom: Bottom,
    split_at: f32,
    on_drag: impl Fn(f32) -> Message + 'a,
) -> Split<'a, Message, Top, Bottom, Theme>
where
    Message: 'a,
    Theme: Catalog + 'a,
{
    Split::new(top, bottom, split_at)
        .direction(Direction::Horizontal)
        .on_drag(on_drag)
}

/// Creates a new [`vertical`](Direction::Vertical) [`Split`] with the given `left` and `right`
/// widgets, a split position, and a function to emit messages when the split gets dragged.
pub fn vertical_split<'a, Message, Left, Right, Theme>(
    left: Left,
    right: Right,
    split_at: f32,
    on_drag: impl Fn(f32) -> Message + 'a,
) -> Split<'a, Message, Left, Right, Theme>
where
    Message: 'a,
    Theme: Catalog + 'a,
{
    Split::new(left, right, split_at).on_drag(on_drag)
}

/// How the [`Split`] is oriented.
#[derive(Clone, Copy, Debug, Default)]
pub enum Direction {
    /// The separator is a horizontal line, separating a top and bottom widget.
    Horizontal,
    /// The separator is a vertical line, separating a left and right widget. This is the
    /// default.
    #[default]
    Vertical,
}

impl Direction {
    fn select<T>(self, x: T, y: T) -> (T, T) {
        match self {
            Self::Horizontal => (x, y),
            Self::Vertical => (y, x),
        }
    }
}

/// How the [`Split`] behaves when dragged or resized.
#[derive(Clone, Copy, Debug, Default)]
pub enum Strategy {
    /// `split_at` is the position of the [`Split`]'s separator relative to its width. This is the
    /// default.
    #[default]
    Relative,
    /// `split_at` is the width of the `start` widget in pixels.
    Start,
    /// `split_at` is the width of the `end` widget in pixels.
    End,
}

struct SplitInfo {
    split_at: f32,
    strategy: Strategy,
    direction: Direction,
    handle_width: f32,
    spacing: f32,
    duration: Duration,
    delay: Duration,
}

/// Resizeable splits for `iced`.
#[expect(missing_debug_implementations)]
pub struct Split<'a, Message, Start, End, Theme>
where
    Message: 'a,
    Theme: Catalog + 'a,
{
    start: Start,
    end: End,
    info: SplitInfo,
    class: Theme::Class<'a>,
    on_drag: Option<Box<dyn Fn(f32) -> Message + 'a>>,
    on_drag_start: Option<Box<dyn Fn() -> Message + 'a>>,
    on_drag_end: Option<Box<dyn Fn() -> Message + 'a>>,
    on_double_click: Option<Box<dyn Fn() -> Message + 'a>>,
}

impl<'a, Message, Start, End, Theme> Split<'a, Message, Start, End, Theme>
where
    Message: 'a,
    Theme: Catalog + 'a,
{
    /// Creates a new [`Split`] with the given `start` and `end` widgets and a split position.
    #[must_use]
    pub fn new(start: Start, end: End, split_at: f32) -> Self {
        Self {
            start,
            end,
            info: SplitInfo {
                split_at,
                strategy: Strategy::default(),
                direction: Direction::default(),
                handle_width: 11.0,
                spacing: 0.0,
                duration: Duration::from_millis(100),
                delay: Duration::from_millis(100),
            },
            class: Theme::default(),
            on_drag: None,
            on_drag_start: None,
            on_drag_end: None,
            on_double_click: None,
        }
    }

    /// Sets the function to emit messages when the [`Split`] gets dragged.
    #[must_use]
    pub fn on_drag(self, on_drag: impl Fn(f32) -> Message + 'a) -> Self {
        self.on_drag_maybe(Some(on_drag))
    }

    /// Sets the function to emit messages when the [`Split`] gets dragged, if `Some`.
    #[must_use]
    pub fn on_drag_maybe(mut self, on_drag_maybe: Option<impl Fn(f32) -> Message + 'a>) -> Self {
        self.on_drag = on_drag_maybe.map(|on_drag| Box::from(on_drag) as _);
        self
    }

    /// Sets the message emitted when the [`Split`] starts getting dragged.
    #[must_use]
    pub fn on_drag_start(self, on_drag_start: Message) -> Self
    where
        Message: Clone,
    {
        self.on_drag_start_maybe(Some(on_drag_start))
    }

    /// Sets the message emitted when the [`Split`] starts getting dragged, if `Some`.
    #[must_use]
    pub fn on_drag_start_maybe(self, on_drag_start_maybe: Option<Message>) -> Self
    where
        Message: Clone,
    {
        self.on_drag_start_with_maybe(
            on_drag_start_maybe.map(|on_drag_start| move || on_drag_start.clone()),
        )
    }

    /// Sets the function to emit messages when the [`Split`] starts getting dragged.
    #[must_use]
    pub fn on_drag_start_with(self, on_drag_start_with: impl Fn() -> Message + 'a) -> Self {
        self.on_drag_start_with_maybe(Some(on_drag_start_with))
    }

    /// Sets the function to emit messages when the [`Split`] starts getting dragged, if `Some`.
    #[must_use]
    pub fn on_drag_start_with_maybe(
        mut self,
        on_drag_start_with_maybe: Option<impl Fn() -> Message + 'a>,
    ) -> Self {
        self.on_drag_start =
            on_drag_start_with_maybe.map(|on_drag_start_with| Box::from(on_drag_start_with) as _);
        self
    }

    /// Sets the message emitted when the [`Split`] finishes getting dragged.
    #[must_use]
    pub fn on_drag_end(self, on_drag_end: Message) -> Self
    where
        Message: Clone,
    {
        self.on_drag_end_maybe(Some(on_drag_end))
    }

    /// Sets the message emitted when the [`Split`] finishes getting dragged, if `Some`.
    #[must_use]
    pub fn on_drag_end_maybe(self, on_drag_end_maybe: Option<Message>) -> Self
    where
        Message: Clone,
    {
        self.on_drag_end_with_maybe(
            on_drag_end_maybe.map(|on_drag_end| move || on_drag_end.clone()),
        )
    }

    /// Sets the function to emit messages when the [`Split`] finishes getting dragged.
    #[must_use]
    pub fn on_drag_end_with(self, on_drag_end_with: impl Fn() -> Message + 'a) -> Self {
        self.on_drag_end_with_maybe(Some(on_drag_end_with))
    }

    /// Sets the function to emit messages when the [`Split`] finishes getting dragged, if `Some`.
    #[must_use]
    pub fn on_drag_end_with_maybe(
        mut self,
        on_drag_end_with_maybe: Option<impl Fn() -> Message + 'a>,
    ) -> Self {
        self.on_drag_end =
            on_drag_end_with_maybe.map(|on_drag_end_with| Box::from(on_drag_end_with) as _);
        self
    }

    /// Sets the message emitted when the [`Split`] is double-clicked.
    #[must_use]
    pub fn on_double_click(self, on_double_click: Message) -> Self
    where
        Message: Clone,
    {
        self.on_double_click_maybe(Some(on_double_click))
    }

    /// Sets the message emitted when the [`Split`] is double-clicked, if `Some`.
    #[must_use]
    pub fn on_double_click_maybe(self, on_double_click_maybe: Option<Message>) -> Self
    where
        Message: Clone,
    {
        self.on_double_click_with_maybe(
            on_double_click_maybe.map(|on_double_click| move || on_double_click.clone()),
        )
    }

    /// Sets the function to emit messages when the [`Split`] is double-clicked.
    #[must_use]
    pub fn on_double_click_with(self, on_double_click_with: impl Fn() -> Message + 'a) -> Self {
        self.on_double_click_with_maybe(Some(on_double_click_with))
    }

    /// Sets the function to emit messages when the [`Split`] is double-clicked, if `Some`.
    #[must_use]
    pub fn on_double_click_with_maybe(
        mut self,
        on_double_click_with_maybe: Option<impl Fn() -> Message + 'a>,
    ) -> Self {
        self.on_double_click = on_double_click_with_maybe
            .map(|on_double_click_with| Box::from(on_double_click_with) as _);
        self
    }

    /// Sets the [`Direction`] of the [`Split`].
    #[must_use]
    pub fn direction(mut self, direction: Direction) -> Self {
        self.info.direction = direction;
        self
    }

    /// Sets the [`Strategy`] of the [`Split`].
    #[must_use]
    pub fn strategy(mut self, strategy: Strategy) -> Self {
        self.info.strategy = strategy;
        self
    }

    /// Sets the width of the [`Split`]'s handle.
    #[must_use]
    pub fn handle_width(mut self, handle_width: impl Into<Pixels>) -> Self {
        self.info.handle_width = handle_width.into().0;
        self
    }

    /// Sets the spacing between the [`Split`]'s handle and content.
    #[must_use]
    pub fn spacing(mut self, spacing: impl Into<Pixels>) -> Self {
        self.info.spacing = spacing.into().0;
        self
    }

    /// Sets the duration of the [`Split`]'s focus and unfocus transitions.
    #[must_use]
    pub fn focus_duration(mut self, duration: Duration) -> Self {
        self.info.duration = duration;
        self
    }

    /// Sets the delay of the [`Split`]'s focus and unfocus transitions.
    #[must_use]
    pub fn focus_delay(mut self, delay: Duration) -> Self {
        self.info.delay = delay;
        self
    }

    /// Sets the [`Style`] of the [`Split`].
    #[must_use]
    pub fn style(mut self, style: impl Fn(&Theme) -> Style + 'a) -> Self
    where
        Theme::Class<'a>: From<StyleFn<'a, Theme>>,
    {
        self.class = (Box::new(style) as StyleFn<'a, Theme>).into();
        self
    }

    /// Sets the [`Class`](Catalog::Class) of the [`Split`].
    #[must_use]
    pub fn class(mut self, class: impl Into<Theme::Class<'a>>) -> Self {
        self.class = class.into();
        self
    }
}

#[derive(Default)]
struct Update {
    capture: bool,
    request_redraw: bool,
    action: Action,
}

#[derive(Default)]
enum Action {
    Drag {
        split_at: f32,
        started: bool,
    },
    DragEnd,
    DoubleClick,
    #[default]
    None,
}

#[derive(PartialEq)]
enum Status {
    Dragging,
    Grabbed,
    DoubleClicked,
    Hovering,
    Idle,
}

struct State {
    status: Status,
    last_click: Option<Click>,
    start_layout: f32,
    mix: Animation<bool>,
    now: Instant,
    duration: Duration,
    delay: Duration,
}

impl State {
    fn new(info: &SplitInfo) -> Self {
        Self {
            status: Status::Idle,
            last_click: None,
            start_layout: 0.0,
            mix: Animation::new(false)
                .duration(info.duration)
                .delay(info.delay),
            now: Instant::now(),
            duration: info.duration,
            delay: info.delay,
        }
    }

    fn diff(&mut self, info: &SplitInfo) {
        if self.duration != info.duration || self.delay != info.delay {
            self.mix = self.mix.clone().duration(info.duration).delay(info.delay);
        }
    }

    fn hovering(&self, bounds: Rectangle, cursor: Cursor, info: &SplitInfo) -> Status {
        let cross_direction = info.direction.select(bounds.width, bounds.height).0;

        let layout = self.start_layout + info.spacing;
        let (x, y) = info.direction.select(0.0, layout);
        let (x, y) = (x + bounds.x, y + bounds.y);
        let (width, height) = info.direction.select(cross_direction, info.handle_width);

        if cursor.is_over(Rectangle {
            x,
            y,
            width,
            height,
        }) {
            Status::Hovering
        } else {
            Status::Idle
        }
    }

    fn update(
        &mut self,
        event: &Event,
        bounds: Rectangle,
        cursor: Cursor,
        is_event_captured: bool,
        info: &SplitInfo,
        draggable: bool,
    ) -> Update {
        let mut update = Update::default();

        if let Event::Window(window::Event::RedrawRequested(now)) = event {
            self.now = *now;

            self.mix
                .go_mut(draggable && self.status != Status::Idle, self.now);
            update.request_redraw = self.mix.is_animating(self.now);

            return update;
        }

        if !draggable || is_event_captured {
            return update;
        }

        if let Event::Mouse(event) = event {
            match event {
                mouse::Event::ButtonPressed(mouse::Button::Left)
                    if self.status == Status::Hovering =>
                {
                    self.last_click = cursor
                        .position()
                        .map(|position| Click::new(position, mouse::Button::Left, self.last_click));

                    self.status = self
                        .last_click
                        .filter(|click| click.kind() == Kind::Double)
                        .map_or(Status::Grabbed, |_| Status::DoubleClicked);

                    update.capture = true;
                }
                mouse::Event::CursorMoved {
                    position: Point { x, y },
                } => match self.status {
                    Status::Dragging | Status::Grabbed | Status::DoubleClicked => {
                        let layout_direction = info.direction.select(bounds.width, bounds.height).1;
                        let split_at = info.direction.select(x - bounds.x, y - bounds.y).1;

                        let separation = 2.0 * info.spacing + info.handle_width;
                        let split_at = match info.strategy {
                            Strategy::Relative => split_at / layout_direction,
                            Strategy::Start => split_at - separation / 2.0,
                            Strategy::End => layout_direction - split_at - separation / 2.0,
                        };

                        if split_at != info.split_at {
                            let started = self.status != Status::Dragging;
                            self.status = Status::Dragging;

                            update.action = Action::Drag { split_at, started };
                            update.capture = true;
                        }
                    }
                    _ => {
                        let focused = self.status != Status::Idle;
                        self.status = self.hovering(bounds, cursor, info);
                        update.request_redraw = (self.status != Status::Idle) != focused;
                    }
                },
                mouse::Event::ButtonReleased(mouse::Button::Left) => match self.status {
                    Status::Dragging => {
                        let focused = self.status != Status::Idle;
                        self.status = self.hovering(bounds, cursor, info);
                        update.request_redraw = (self.status != Status::Idle) != focused;
                        update.action = Action::DragEnd;
                    }
                    Status::Grabbed => self.status = Status::Hovering,
                    Status::DoubleClicked => {
                        self.status = Status::Hovering;
                        update.action = Action::DoubleClick;
                    }
                    _ => {}
                },
                _ => {}
            }
        }

        update
    }

    fn separator(
        &self,
        style: &Style,
        bounds: Rectangle,
        hint_factor: Option<f32>,
        info: &SplitInfo,
    ) -> (Quad, Color) {
        let color = self
            .mix
            .interpolate(style.unfocused.color, style.focused.color, self.now);

        let width = self
            .mix
            .interpolate(style.unfocused.width, style.focused.width, self.now);

        let radius = Radius {
            top_left: self.mix.interpolate(
                style.unfocused.radius.top_left,
                style.focused.radius.top_left,
                self.now,
            ),
            top_right: self.mix.interpolate(
                style.unfocused.radius.top_right,
                style.focused.radius.top_right,
                self.now,
            ),
            bottom_right: self.mix.interpolate(
                style.unfocused.radius.bottom_right,
                style.focused.radius.bottom_right,
                self.now,
            ),
            bottom_left: self.mix.interpolate(
                style.unfocused.radius.bottom_left,
                style.focused.radius.bottom_left,
                self.now,
            ),
        };

        let cross_direction = info.direction.select(bounds.width, bounds.height).0;
        let layout = self.start_layout + info.spacing + (info.handle_width - width) / 2.0;
        let (x, y) = info.direction.select(0.0, layout);
        let (x, y) = (x + bounds.x, y + bounds.y);
        let (width, height) = info.direction.select(cross_direction, width);
        let (width, height) = if style.snap {
            let unit = 1.0 / hint_factor.unwrap_or(1.0);
            (width.max(unit), height.max(unit))
        } else {
            (width, height)
        };

        let quad = Quad {
            bounds: Rectangle {
                x,
                y,
                width,
                height,
            },
            border: border::rounded(radius),
            snap: style.snap,
            ..Quad::default()
        };

        (quad, color)
    }
}

impl<'a, Message, Start, End, Theme> Meta for Split<'a, Message, Start, End, Theme>
where
    Message: 'a,
    Theme: Catalog + 'a,
{
}

impl<'a, Message, Start, End, Theme, Renderer> Widget<Message, Theme, Renderer>
    for Split<'a, Message, Start, End, Theme>
where
    Message: 'a,
    Theme: Catalog + 'a,
    Renderer: iced_core::Renderer,
    Start: Widget<Message, Theme, Renderer>,
    End: Widget<Message, Theme, Renderer>,
{
    fn size(&self) -> Size<Length> {
        Size::new(Length::Fill, Length::Fill)
    }

    fn tag(&self) -> tree::Tag {
        tree::Tag::of::<State>()
    }

    fn state(&self) -> tree::State {
        tree::State::new(State::new(&self.info))
    }

    fn diff(&mut self, tree: &mut Tree) {
        tree.state.downcast_mut::<State>().diff(&self.info);

        match tree.children.len() {
            0 => tree
                .children
                .extend([Tree::new(&self.start), Tree::new(&self.end)]),
            1 => tree.children.push(Tree::new(&self.end)),
            _ => tree.children.truncate(2),
        }

        tree.children[0].diff(&mut self.start);
        tree.children[1].diff(&mut self.end);
    }

    fn layout(&mut self, tree: &mut Tree, renderer: &Renderer, limits: &Limits) {
        let min = |size: Size<Length>| match self.info.direction.select(size.width, size.height).1 {
            Length::Fixed(min)
            | Length::Bounded {
                bounds: Bounds::Min(min) | Bounds::Both { min, .. },
                ..
            }
            | Length::Fluid(Constraint::Min(min)) => min,
            _ => 0.0,
        };

        let start_min = min(self.start.size());
        let end_min = min(self.end.size());

        let (cross_direction, layout_direction) = self
            .info
            .direction
            .select(limits.max.width, limits.max.height);

        let separation = 2.0 * self.info.spacing + self.info.handle_width;
        let state = tree.state.downcast_mut::<State>();
        state.start_layout = match self.info.strategy {
            Strategy::Relative => layout_direction * self.info.split_at - separation / 2.0,
            Strategy::Start => self.info.split_at,
            Strategy::End => layout_direction - self.info.split_at - separation,
        }
        .min(layout_direction - separation - end_min)
        .max(start_min);

        let (start_width, start_height) = self
            .info
            .direction
            .select(cross_direction, state.start_layout);
        let start_limits = Limits::new(Size::ZERO, Size::new(start_width, start_height));

        self.start
            .layout(&mut tree.children[0], renderer, &start_limits);

        let end_layout = layout_direction - state.start_layout - separation;
        let (end_width, end_height) = self.info.direction.select(cross_direction, end_layout);
        let end_limits = Limits::new(Size::ZERO, Size::new(end_width, end_height));

        self.end
            .layout(&mut tree.children[1], renderer, &end_limits);

        let (offset_width, offset_height) = self
            .info
            .direction
            .select(0.0, state.start_layout + separation);

        tree.children[0].translation = Vector::ZERO;
        tree.children[1].translation = Vector::new(offset_width, offset_height);

        tree.size = limits.max;
    }

    fn update(
        &mut self,
        tree: &mut Tree,
        event: &Event,
        layout: Layout,
        cursor: Cursor,
        renderer: &Renderer,
        shell: &mut Shell<'_, Message>,
        viewport: &Rectangle,
    ) {
        let mut iter = layout.iter_mut(&mut tree.children);

        {
            let (layout, tree) = iter.next().unwrap();
            self.start
                .update(tree, event, layout, cursor, renderer, shell, viewport);
        }

        {
            let (layout, tree) = iter.next().unwrap();
            self.end
                .update(tree, event, layout, cursor, renderer, shell, viewport);
        }

        let update = tree.state.downcast_mut::<State>().update(
            event,
            layout.bounds(),
            cursor,
            shell.is_event_captured(),
            &self.info,
            self.on_drag.is_some(),
        );

        match update.action {
            Action::Drag { split_at, started } => {
                if started && let Some(on_drag_start) = &self.on_drag_start {
                    shell.publish(on_drag_start());
                }

                if let Some(on_drag) = &self.on_drag {
                    shell.publish(on_drag(split_at));
                }
            }
            Action::DragEnd => {
                if let Some(on_drag_end) = &self.on_drag_end {
                    shell.publish(on_drag_end());
                    shell.capture_event();
                }
            }
            Action::DoubleClick => {
                if let Some(on_double_click) = &self.on_double_click {
                    shell.publish(on_double_click());
                    shell.capture_event();
                }
            }
            Action::None => {}
        }

        if update.capture {
            shell.capture_event();
        }

        if update.request_redraw {
            shell.request_redraw();
        }
    }

    fn draw(
        &self,
        tree: &Tree,
        renderer: &mut Renderer,
        theme: &Theme,
        style: &renderer::Style,
        layout: Layout,
        cursor: Cursor,
        viewport: &Rectangle,
    ) {
        let mut iter = layout.iter(&tree.children);

        {
            let (layout, tree) = iter.next().unwrap();
            self.start
                .draw(tree, renderer, theme, style, layout, cursor, viewport);
        }

        {
            let (layout, tree) = iter.next().unwrap();
            self.end
                .draw(tree, renderer, theme, style, layout, cursor, viewport);
        }

        let (quad, color) = tree.state.downcast_ref::<State>().separator(
            &theme.style(&self.class),
            layout.bounds(),
            renderer.hint_factor(),
            &self.info,
        );

        renderer.fill_quad(quad, color);
    }

    fn mouse_interaction(
        &self,
        tree: &Tree,
        layout: Layout,
        cursor: Cursor,
        viewport: &Rectangle,
        renderer: &Renderer,
    ) -> Interaction {
        if tree.state.downcast_ref::<State>().status == Status::Idle {
            let mut iter = layout.iter(&tree.children);

            let mut mouse_interaction = {
                let (layout, tree) = iter.next().unwrap();
                self.start
                    .mouse_interaction(tree, layout, cursor, viewport, renderer)
            };

            mouse_interaction = mouse_interaction.max({
                let (layout, tree) = iter.next().unwrap();
                self.end
                    .mouse_interaction(tree, layout, cursor, viewport, renderer)
            });

            mouse_interaction
        } else {
            match self.info.direction {
                Direction::Horizontal => Interaction::ResizingRow,
                Direction::Vertical => Interaction::ResizingColumn,
            }
        }
    }

    fn overlay<'b>(
        &'b mut self,
        tree: &'b mut Tree,
        layout: Layout,
        renderer: &Renderer,
        viewport: &Rectangle,
        translation: Vector,
        window: Size,
    ) -> Vec<overlay::Element<'b, Message, Theme, Renderer>> {
        let mut iter = layout.iter_mut(&mut tree.children);

        let mut overlay = {
            let (layout, tree) = iter.next().unwrap();
            self.start
                .overlay(tree, layout, renderer, viewport, translation, window)
        };

        overlay.extend({
            let (layout, tree) = iter.next().unwrap();
            self.end
                .overlay(tree, layout, renderer, viewport, translation, window)
        });

        overlay
    }

    fn operate(
        &mut self,
        tree: &mut Tree,
        layout: Layout,
        viewport: &Rectangle,
        renderer: &Renderer,
        operation: &mut dyn Operation,
    ) {
        operation.container(None, layout.bounds(), viewport);
        operation.traverse(&mut |operation| {
            let mut iter = layout.iter_mut(&mut tree.children);

            {
                let (layout, tree) = iter.next().unwrap();
                self.start
                    .operate(tree, layout, viewport, renderer, operation);
            }

            {
                let (layout, tree) = iter.next().unwrap();
                self.end
                    .operate(tree, layout, viewport, renderer, operation);
            }
        });
    }
}

/// The [style](Style) of a [`Split`].
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Style {
    /// The [`StyleSheet`] of the [`Split`] while it's unfocused.
    pub unfocused: StyleSheet,
    /// The [`StyleSheet`] of the [`Split`] while it's focused.
    pub focused: StyleSheet,
    /// Whether the separator should be snapped to the pixel grid.
    pub snap: bool,
}

/// The [stylesheet](StyleSheet) of a [`Split`].
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct StyleSheet {
    /// The color of the separator.
    pub color: Color,
    /// The width of the separator.
    pub width: f32,
    /// The radius of the corners of the separator.
    pub radius: Radius,
}

/// The [theme catalog](Catalog) of a [`Split`].
pub trait Catalog {
    /// The [`item class`](Self::Class) of the [`Catalog`].
    type Class<'a>;

    /// The default [`Class`](Self::Class) produced by the [`Catalog`].
    fn default<'a>() -> Self::Class<'a>;

    /// The [`Style`] of a [`Class`](Self::Class).
    fn style(&self, class: &Self::Class<'_>) -> Style;
}

/// A styling function for a [`Split`].
pub type StyleFn<'a, Theme> = Box<dyn Fn(&Theme) -> Style + 'a>;

impl Catalog for iced_core::Theme {
    type Class<'a> = StyleFn<'a, Self>;

    fn default<'a>() -> Self::Class<'a> {
        Box::new(default)
    }

    fn style(&self, class: &Self::Class<'_>) -> Style {
        class(self)
    }
}

/// The default styling of a [`Split`].
#[must_use]
pub fn default(theme: &iced_core::Theme) -> Style {
    let palette = theme.palette();

    Style {
        unfocused: StyleSheet {
            color: palette.background.strong.color,
            width: 1.0,
            radius: 0.5.into(),
        },
        focused: StyleSheet {
            color: palette.primary.base.color,
            width: 5.0,
            radius: 2.5.into(),
        },
        snap: true,
    }
}
