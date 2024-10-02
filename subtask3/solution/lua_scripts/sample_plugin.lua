-- scripts/test_plugin.lua

register_command("insert_test", function()
    editor.insert_text("Test plugin executed.\n")
end)

register_command("quit_test", function()
    editor.quit()
end)
