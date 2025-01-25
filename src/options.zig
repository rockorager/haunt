const ghostty = @import("ghostty");
const Terminal = @import("Terminal");

pub const ghostty_options = struct {
    pub const Renderer = Terminal.Renderer;
    pub const runtime = Terminal;
};
