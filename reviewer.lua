function Div(el)
  -- Check if the div has the 'reviewer' class
  if el.classes:includes('reviewer') then
    -- For Typst format, wrap content in #text(fill: blue)[...]
    if FORMAT:match 'typst' then
      return {
        pandoc.RawBlock('typst', '#text(fill: blue)['),
        el,
        pandoc.RawBlock('typst', ']')
      }
    end
  end
  return el
end

function Span(el)
  -- Check if the span has the 'reviewer' class
  if el.classes:includes('reviewer') then
    -- For Typst format, wrap content in #text(fill: blue)[...]
    if FORMAT:match 'typst' then
      return {
        pandoc.RawInline('typst', '#text(fill: blue)['),
        el,
        pandoc.RawInline('typst', ']')
      }
    end
  end
  return el
end
