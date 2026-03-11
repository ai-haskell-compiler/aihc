--
-- Tests for the doclayout module
--
local doclayout = require 'doclayout'
local tasty = require 'tasty'

local group = tasty.test_group
local test = tasty.test_case
local assert = tasty.assert

local function renderANSI (doc, cols)
  return doclayout.render(doc, cols, 'ansi')
end

-- Check existence static fields
return {
  group 'constructors' {
    test('empty', function ()
      assert.are_equal(type(doclayout.empty), 'userdata')
    end),
    test('blankline', function ()
      assert.are_equal(type(doclayout.blankline), 'userdata')
    end),
    test('cr', function ()
      assert.are_equal(type(doclayout.cr), 'userdata')
    end),
    test('space', function ()
      assert.are_equal(type(doclayout.space), 'userdata')
    end),

    test('chomp', function ()
      local doc = 'a' .. doclayout.blanklines(2)
      assert.are_equal(doclayout.chomp(doc), doclayout.literal 'a')
    end),
    test('nestle', function ()
      local doc = doclayout.blanklines(3) .. 'a'
      assert.are_equal(doclayout.nestle(doc), doclayout.literal 'a')
    end),
    group 'enclosing' {
      test('quotes', function ()
        local doc = doclayout.literal 'single'
        assert.are_equal(
          doclayout.quotes(doc),
          "'" .. doclayout.literal 'single' .. "'"
        )
      end),
      test('double quotes', function ()
        local doc = doclayout.literal 'double'
        assert.are_equal(
          doclayout.double_quotes(doc),
          '"' .. doclayout.literal 'double' .. '"'
        )
      end),
      test('inside', function ()
        local doc = doclayout.literal 'Hello,' + 'World!'
        assert.are_equal(
          doclayout.inside(doc, 'Yo! ', ' Wassup?'),
          'Yo! ' .. doc .. ' Wassup?'
        )
      end),
    },
    group 'concat' {
      test('with sep', function ()
        local list = {
          doclayout.literal 'one',
          doclayout.literal 'two',
          doclayout.literal 'three'
        }
        local sep = doclayout.cr
        assert.are_equal(
          doclayout.concat(list, sep),
          list[1] .. sep .. list[2] .. sep .. list[3])
      end),
      test('without sep', function ()
        local list = {
          doclayout.literal 'one',
          doclayout.literal 'two',
          doclayout.literal 'three'
        }
        assert.are_equal(doclayout.concat(list), list[1] .. list[2] .. list[3])
      end),
    },
  },

  group 'render' {
    test('empty doc', function ()
      assert.are_equal(doclayout.render(doclayout.empty), '')
    end),
    test('reflow', function ()
      local greeting = doclayout.literal 'Hi!' .. doclayout.space
        .. doclayout.literal 'How' .. doclayout.space
        .. doclayout.literal 'are' .. doclayout.space
        .. doclayout.literal 'you?'
      assert.are_equal(greeting:render(7), 'Hi! How\nare\nyou?')
    end),
    test('after_break', function ()
      local doc = doclayout.literal 'hi'
        + doclayout.after_break '!'
        .. doclayout.after_break '?'
        .. doclayout.literal 'x'
        .. doclayout.after_break '?'
      assert.are_equal(doc:render(2), 'hi\n!x')
    end),
    test('before_non_blank', function ()
      local doc = doclayout.before_non_blank '!!' .. ' ab'
        / doclayout.before_non_blank '!!' .. 'a b'
      assert.are_equal(doclayout.render(doc), ' ab\n!!a b')
    end),
    test('blanks at beginning', function ()
      local doc = doclayout.blanklines(2) .. 'aa'
      -- only one newline, as the top of doc is treated as implicit blank
      assert.are_equal(doclayout.render(doc), '\naa')
    end),
    test('blanklines', function ()
      local doc = 'aa' .. doclayout.blanklines(2) .. 'bb'
      assert.are_equal(doclayout.render(doc), 'aa\n\n\nbb')
    end),
    test('braces', function ()
      local doc = doclayout.braces 'maybe'
      assert.are_equal(doclayout.render(doc), '{maybe}')
    end),
    test('brackets', function ()
      local doc = doclayout.brackets '1'
      assert.are_equal(doclayout.render(doc), '[1]')
    end),
    test('flush', function ()
      local doc = doclayout.flush(doclayout.nest('hi', 2))
      assert.are_equal(doclayout.render(doc), 'hi')
    end),
    test('hang', function ()
      local doc = doclayout.hang('aa\nbb\ncc', 4, '  - ')
      assert.are_equal(
        doclayout.render(doc),
        table.concat{
          '  - aa\n',
          '    bb\n',
          '    cc',
        }
      )
    end),
    test('nest', function ()
      local doc = doclayout.nest('aa\n\nbb\ncc', 2)
      assert.are_equal(
        doclayout.render(doc),
        table.concat{
          '  aa\n',
          '\n',
          '  bb\n',
          '  cc'
        }
      )
    end),
    test('nowrap', function()
      local doc = doclayout.nowrap(doclayout.literal 'first' + 'second')
      assert.are_equal(doc:render(8), 'first second')
    end),
    test('parens', function ()
      local doc = doclayout.parens 'lisp'
      assert.are_equal(doclayout.render(doc), '(lisp)')
      -- as method
      assert.are_equal(doc:parens():render(), '((lisp))')
    end),
    test('prefixed', function ()
      local doc = doclayout.prefixed(doclayout.literal 'aa' // 'bb', '# ' )
      assert.are_equal(doclayout.render(doc), '# aa\n#\n# bb')
      -- as method
      assert.are_equal(
        (doclayout.literal'aa' // 'bb'):prefixed('# '):render(),
        '# aa\n#\n# bb'
      )
    end),
    group 'table helpers' {
      test('cblock', function ()
        local doc = doclayout.cblock('| ', 2)
          .. doclayout.cblock('aa', 4)
          .. doclayout.cblock(' |', 2)
        assert.are_equal(doclayout.render(doc), '|  aa  |')
      end),
      test('lblock', function ()
        local doc = doclayout.lblock('| ', 2)
          .. doclayout.lblock('aa', 4)
          .. doclayout.lblock(' |', 2)
        assert.are_equal(doclayout.render(doc), '| aa   |')
      end),
      test('rblock', function ()
        local doc = doclayout.rblock('| ', 2)
          .. doclayout.rblock('aa', 4)
          .. doclayout.rblock(' |', 2)
        assert.are_equal(doclayout.render(doc), '|   aa |')
      end),
      test('vfill', function ()
        local doc = doclayout.vfill '| '
          .. doclayout.lblock(doclayout.literal 'aa' // 'bbb', 4)
          .. doclayout.vfill(' |')
        assert.are_equal(
          doclayout.render(doc),
          table.concat{
            '| aa   |\n',
            '|      |\n',
            '| bbb  |'
          }
        )
      end)
    }
  },

  group 'document querying' {
    test('is_empty', function ()
      assert.is_truthy(doclayout.is_empty(doclayout.empty))
      assert.is_truthy(doclayout.is_empty(''))
      assert.is_falsy(doclayout.is_empty('non-empty'))
    end),

    test('height', function ()
      assert.are_equal(doclayout.height(doclayout.empty), 1)
      assert.are_equal(doclayout.height('line'), 1)
      assert.are_equal(doclayout.height(doclayout.literal 'p' / 'q'), 2)
      assert.are_equal(doclayout.height(doclayout.literal 'p' // 'q'), 3)
    end),

    test('min_offset', function ()
      assert.are_equal(doclayout.min_offset 'four', 4)
      assert.are_equal(
        doclayout.min_offset(doclayout.literal 'four' + 'radio'),
        5
      )
    end),

    test('offset', function ()
      assert.are_equal(doclayout.offset 'four', 4)
      assert.are_equal(doclayout.offset(doclayout.literal 'four' / 'radio'), 5)
    end),

    test('real_length', function ()
      assert.are_equal(doclayout.real_length(''), 0)
      assert.are_equal(doclayout.real_length('a'), 1)
      assert.are_equal(doclayout.real_length('❄'), 1)
      assert.are_equal(doclayout.real_length('シ'), 2)
      assert.are_equal(doclayout.real_length('four'), 4)
    end),

    test('update_column', function ()
      local doc = 'long' .. doclayout.cr .. 'longer'
      assert.are_equal(doclayout.update_column(doc, 0), 6)
      -- fails with doclayout < 0.4:
      -- assert.are_equal(doclayout.update_column(doclayout.empty, 42), 42)
      assert.are_equal(doclayout.update_column('four', 4), 8)
    end)
  },

  -- Styling happens per-character, so the output is quite verbose, and
  -- includes additional commands to reset all attributes.
  -- We just look for the right escape sequences instead of matching
  -- the whole output.
  group 'styling' {
    test('bold', function ()
      assert.is_truthy(
        renderANSI(doclayout.bold('a')):match('\027%[1m')
      )
    end),

    test('italics', function ()
      assert.is_truthy(
        renderANSI(doclayout.italic('a')):match('\027%[3m')
      )
    end),

    test('underlined', function ()
      assert.is_truthy(
        renderANSI(doclayout.underlined('a')):match('\027%[4m')
      )
    end),

    test('strikeout', function ()
      assert.is_truthy(
        renderANSI(doclayout.strikeout('a')):match('\027%[9m')
      )
    end),

    test('fg', function ()
      assert.is_truthy(
        renderANSI(doclayout.fg('a', 'red')):match('\027%[31m')
      )
    end),

    test('bg', function ()
      assert.is_truthy(
        renderANSI(doclayout.bg('a', 'red')):match('\027%[41m')
           )
    end),

    group 'colors' {
      test('black', function ()
        renderANSI(doclayout.bg('a', 'black')):match('\027%[40m')
      end),
      test('red', function ()
        renderANSI(doclayout.bg('a', 'red')):match('\027%[41m')
      end),
      test('green', function ()
        renderANSI(doclayout.bg('a', 'green')):match('\027%[42m')
      end),
      test('yellow', function ()
        renderANSI(doclayout.bg('a', 'yellow')):match('\027%[43m')
      end),
      test('blue', function ()
        renderANSI(doclayout.bg('a', 'blue')):match('\027%[44m')
      end),
      test('magenta', function ()
        renderANSI(doclayout.bg('a', 'magenta')):match('\027%[45m')
      end),
      test('cyan', function ()
        renderANSI(doclayout.bg('a', 'cyan')):match('\027%[46m')
      end),
      test('white', function ()
        renderANSI(doclayout.bg('a', 'white')):match('\027%[47m')
      end),
    }
  },

  group 'Doc type' {
    test('empty strings equal the empty Doc', function ()
      assert.are_equal(doclayout.empty .. '', doclayout.empty)
    end),

    test('strings can be used as Doc values', function ()
      assert.are_equal(doclayout.render('hello world'), 'hello world')
    end),

    test('numbers can be used as Doc values', function ()
      assert.are_equal(doclayout.render(42), '42')
    end),

    test('equality', function ()
      assert.is_truthy(doclayout.literal "true", doclayout.literal "true")
    end),

    test('concatenate docs', function ()
      assert.are_equal(
        tostring(doclayout.literal 'Rock-' .. doclayout.literal 'Ola'),
       'Rock-Ola'
      )
    end),

    test('has tostring method', function ()
      local str = 'just a literal string for now'
      assert.are_equal(tostring(str), str)
    end),

    test('adding concatenates with space', function ()
      local helloworld = doclayout.literal 'Hello,'
        .. doclayout.space
        .. doclayout.literal 'World'
      assert.are_equal(doclayout.literal 'Hello,' + 'World', helloworld)
    end),

    test('dividing sets the first above the second', function ()
           local first = doclayout.literal 'first'
           local second = doclayout.literal 'second'
           assert.are_equal(first / second, first .. doclayout.cr .. second)
    end),

    test('// separates docs with blank line', function ()
      local first = doclayout.literal 'first'
      local second = doclayout.literal 'second'
      assert.are_equal(first // second, first .. doclayout.blankline .. second)
    end),
  }
}
