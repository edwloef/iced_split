use iced_widget::{
    core::{
        self, Clipboard, Element, Event, Layout, Length, Point, Rectangle, Shell, Size, Vector,
        Widget, border,
        layout::{Limits, Node},
        mouse::{self, Cursor, Interaction},
        overlay,
        renderer::{Quad, Style},
        widget::{Operation, Tree, tree},
    },
    rule,
};

#[derive(Clone, Copy, Debug, Default)]
pub enum Strategy {
    #[default]
    Relative,
    Start,
    End,
}

#[derive(Clone, Copy, Debug, Default)]
pub enum Direction {
    Horizontal,
    #[default]
    Vertical,
}

#[derive(Default)]
struct State {
    hovering: bool,
    dragging: bool,
}

#[expect(missing_debug_implementations, clippy::struct_field_names)]
pub struct Split<'a, Message, Theme = iced_widget::Theme, Renderer = iced_widget::Renderer>
where
    Theme: rule::Catalog,
{
    children: [Element<'a, Message, Theme, Renderer>; 2],
    split_at: f32,
    strategy: Strategy,
    direction: Direction,
    thickness: f32,
    class: Theme::Class<'a>,
    f: Box<dyn Fn(f32) -> Message + 'a>,
}

impl<'a, Message, Theme, Renderer> Split<'a, Message, Theme, Renderer>
where
    Theme: rule::Catalog,
{
    #[must_use]
    pub fn new(
        start: impl Into<Element<'a, Message, Theme, Renderer>>,
        end: impl Into<Element<'a, Message, Theme, Renderer>>,
        split_at: f32,
        f: impl Fn(f32) -> Message + 'a,
    ) -> Self {
        Self {
            children: [start.into(), end.into()],
            split_at,
            strategy: Strategy::default(),
            direction: Direction::default(),
            thickness: 11.0,
            class: Theme::default(),
            f: Box::from(f),
        }
    }

    #[must_use]
    pub fn direction(mut self, direction: Direction) -> Self {
        self.direction = direction;
        self
    }

    #[must_use]
    pub fn strategy(mut self, strategy: Strategy) -> Self {
        self.strategy = strategy;
        self
    }

    #[must_use]
    pub fn thickness(mut self, thickness: f32) -> Self {
        self.thickness = thickness;
        self
    }

    #[must_use]
    pub fn style(mut self, style: impl Fn(&Theme) -> rule::Style + 'a) -> Self
    where
        Theme::Class<'a>: From<rule::StyleFn<'a, Theme>>,
    {
        self.class = (Box::new(style) as rule::StyleFn<'a, Theme>).into();
        self
    }

    #[must_use]
    pub fn class(mut self, class: impl Into<Theme::Class<'a>>) -> Self {
        self.class = class.into();
        self
    }
}

impl<Message, Theme, Renderer> Widget<Message, Theme, Renderer>
    for Split<'_, Message, Theme, Renderer>
where
    Theme: rule::Catalog,
    Renderer: core::Renderer,
{
    fn children(&self) -> Vec<Tree> {
        self.children.iter().map(Tree::new).collect()
    }

    fn size(&self) -> Size<Length> {
        Size::new(Length::Fill, Length::Fill)
    }

    fn tag(&self) -> tree::Tag {
        tree::Tag::of::<State>()
    }

    fn state(&self) -> tree::State {
        tree::State::new(State::default())
    }

    fn diff(&self, tree: &mut Tree) {
        tree.diff_children(&self.children);
    }

    fn layout(&self, tree: &mut Tree, renderer: &Renderer, limits: &Limits) -> Node {
        let max_limits = limits.max();

        let (cross_direction, layout_direction) = match self.direction {
            Direction::Horizontal => (max_limits.width, max_limits.height),
            Direction::Vertical => (max_limits.height, max_limits.width),
        };

        let start_layout = match self.strategy {
            Strategy::Relative => layout_direction.mul_add(self.split_at, -self.thickness / 2.0),
            Strategy::Start => self.split_at,
            Strategy::End => layout_direction - self.split_at - self.thickness,
        };
        let (start_width, start_height) = match self.direction {
            Direction::Horizontal => (cross_direction, start_layout),
            Direction::Vertical => (start_layout, cross_direction),
        };
        let start_limits = Limits::new(Size::new(0.0, 0.0), Size::new(start_width, start_height));

        let end_layout = layout_direction - start_layout - self.thickness;
        let (end_width, end_height) = match self.direction {
            Direction::Horizontal => (cross_direction, end_layout),
            Direction::Vertical => (end_layout, cross_direction),
        };
        let end_limits = Limits::new(Size::new(0.0, 0.0), Size::new(end_width, end_height));

        let (offset_width, offset_height) = match self.direction {
            Direction::Horizontal => (0.0, start_layout + self.thickness),
            Direction::Vertical => (start_layout + self.thickness, 0.0),
        };

        let children = vec![
            self.children[0]
                .as_widget()
                .layout(&mut tree.children[0], renderer, &start_limits),
            self.children[1]
                .as_widget()
                .layout(&mut tree.children[1], renderer, &end_limits)
                .translate(Vector::new(offset_width, offset_height)),
        ];

        Node::with_children(max_limits, children)
    }

    fn update(
        &mut self,
        tree: &mut Tree,
        event: &Event,
        layout: Layout<'_>,
        cursor: Cursor,
        renderer: &Renderer,
        clipboard: &mut dyn Clipboard,
        shell: &mut Shell<'_, Message>,
        viewport: &Rectangle,
    ) {
        self.children
            .iter_mut()
            .zip(&mut tree.children)
            .zip(layout.children())
            .for_each(|((child, tree), layout)| {
                child.as_widget_mut().update(
                    tree, event, layout, cursor, renderer, clipboard, shell, viewport,
                );
            });

        if shell.is_event_captured() {
            return;
        }

        let state = tree.state.downcast_mut::<State>();
        let bounds = layout.bounds();

        if let Event::Mouse(event) = event {
            match event {
                mouse::Event::ButtonPressed(mouse::Button::Left) if state.hovering => {
                    state.dragging = true;
                    shell.capture_event();
                }
                mouse::Event::CursorMoved {
                    position: Point { x, y },
                    ..
                } => {
                    let (cross_direction, layout_direction) = match self.direction {
                        Direction::Horizontal => (bounds.width, bounds.height),
                        Direction::Vertical => (bounds.height, bounds.width),
                    };

                    if state.dragging {
                        let relative_position = (match self.direction {
                            Direction::Horizontal => y - bounds.y,
                            Direction::Vertical => x - bounds.x,
                        } - self.thickness / 2.0)
                            .max(0.0)
                            .min(layout_direction - self.thickness);

                        let split_at = match self.strategy {
                            Strategy::Relative => {
                                (relative_position + self.thickness / 2.0) / layout_direction
                            }
                            Strategy::Start => relative_position,
                            Strategy::End => layout_direction - relative_position - self.thickness,
                        };

                        shell.publish((self.f)(split_at));
                        shell.capture_event();
                    }

                    let layout = match self.strategy {
                        Strategy::Relative => {
                            layout_direction.mul_add(self.split_at, -self.thickness / 2.0)
                        }
                        Strategy::Start => self.split_at,
                        Strategy::End => layout_direction - self.split_at - self.thickness,
                    };

                    let (x, y, width, height) = match self.direction {
                        Direction::Horizontal => (0.0, layout, cross_direction, self.thickness),
                        Direction::Vertical => (layout, 0.0, self.thickness, cross_direction),
                    };

                    let bounds = Rectangle {
                        x,
                        y,
                        width,
                        height,
                    } + Vector::new(bounds.x, bounds.y);

                    state.hovering = cursor.is_over(bounds);
                }
                mouse::Event::ButtonReleased(mouse::Button::Left) if state.dragging => {
                    state.dragging = false;
                    shell.capture_event();
                }
                _ => {}
            }
        }
    }

    fn draw(
        &self,
        tree: &Tree,
        renderer: &mut Renderer,
        theme: &Theme,
        style: &Style,
        layout: Layout<'_>,
        cursor: Cursor,
        viewport: &Rectangle,
    ) {
        self.children
            .iter()
            .zip(&tree.children)
            .zip(layout.children())
            .for_each(|((child, tree), layout)| {
                child
                    .as_widget()
                    .draw(tree, renderer, theme, style, layout, cursor, viewport);
            });

        let bounds = layout.bounds();
        let style = theme.style(&self.class);

        let (cross_direction, layout_direction) = match self.direction {
            Direction::Horizontal => (bounds.width, bounds.height),
            Direction::Vertical => (bounds.height, bounds.width),
        };

        let layout = match self.strategy {
            Strategy::Relative => layout_direction.mul_add(self.split_at, -self.thickness / 2.0),
            Strategy::Start => self.split_at,
            Strategy::End => layout_direction - self.split_at - self.thickness,
        } + self.thickness / 2.0;

        let width = f32::from(style.width);
        let (offset, length) = style.fill_mode.fill(cross_direction);

        let (x, y, width, height) = match self.direction {
            Direction::Horizontal => (0.0, width.mul_add(-0.5, layout + offset), length, width),
            Direction::Vertical => (width.mul_add(-0.5, layout + offset), 0.0, width, length),
        };

        let bounds = Rectangle {
            x,
            y,
            width,
            height,
        } + Vector::new(bounds.x, bounds.y);

        renderer.fill_quad(
            Quad {
                bounds,
                border: border::rounded(style.radius),
                snap: style.snap,
                ..Quad::default()
            },
            style.color,
        );
    }

    fn mouse_interaction(
        &self,
        tree: &Tree,
        layout: Layout<'_>,
        cursor: Cursor,
        viewport: &Rectangle,
        renderer: &Renderer,
    ) -> Interaction {
        let state = tree.state.downcast_ref::<State>();

        if state.hovering || state.dragging {
            match self.direction {
                Direction::Horizontal => Interaction::ResizingVertically,
                Direction::Vertical => Interaction::ResizingHorizontally,
            }
        } else {
            self.children
                .iter()
                .zip(&tree.children)
                .zip(layout.children())
                .map(|((child, tree), layout)| {
                    child
                        .as_widget()
                        .mouse_interaction(tree, layout, cursor, viewport, renderer)
                })
                .max()
                .unwrap_or_default()
        }
    }

    fn overlay<'a>(
        &'a mut self,
        tree: &'a mut Tree,
        layout: Layout<'a>,
        renderer: &Renderer,
        viewport: &Rectangle,
        translation: Vector,
    ) -> Option<overlay::Element<'a, Message, Theme, Renderer>> {
        overlay::from_children(
            &mut self.children,
            tree,
            layout,
            renderer,
            viewport,
            translation,
        )
    }

    fn operate(
        &self,
        tree: &mut Tree,
        layout: Layout<'_>,
        renderer: &Renderer,
        operation: &mut dyn Operation,
    ) {
        operation.container(None, layout.bounds(), &mut |operation| {
            self.children
                .iter()
                .zip(&mut tree.children)
                .zip(layout.children())
                .for_each(|((child, state), layout)| {
                    child
                        .as_widget()
                        .operate(state, layout, renderer, operation);
                });
        });
    }
}

impl<'a, Message, Theme, Renderer> From<Split<'a, Message, Theme, Renderer>>
    for Element<'a, Message, Theme, Renderer>
where
    Message: 'a,
    Theme: rule::Catalog + 'a,
    Renderer: core::Renderer + 'a,
{
    fn from(value: Split<'a, Message, Theme, Renderer>) -> Self {
        Self::new(value)
    }
}
