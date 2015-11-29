
$levels = {}

$levels["0-easy.level"] = <<-EOF
.468.2.51
8.....9..
.19354...
.3......8
6..7.5..2
1......3.
...48769.
..4.....7
58.9.124.
EOF

$levels["1-medium.level"] = <<-EOF
12......6
.53......
.8.....5.
...48.7.5
8..9.7..2
3.7.25...
.1.....7.
......41.
6......23
EOF

$levels["2-hard.level"] = <<-EOF
....4.3..
4..3.....
.....74.8
.......47
.........
24.......
5.12.....
.....4..1
..4.6....
EOF

$levels["3-test.level"] = <<-EOF
.91286574
487359126
652714839
875431692
213967485
964528713
149673258
538142967
726895341
EOF

def parse_string(str)
  str = str.split("\n").map {|s| s.split('')}.flatten.map do |n|
    n == '.' ? nil : n.to_i
  end
  res = Array.new 9, []

  i, line = [0, 0]
  str.each_slice(3) do |a|
    res[(line / 3) * 3 + i] += a 

    i = (i + 1) % 3
    line += 1 if i == 0
  end

  res
end

def compile(a)
  "[#{a.map(&method(:compile_inner)).join(", ")}]"
end

def compile_inner(a)
  "[#{a.map {|n| n.nil? ? "Right Nothing" : "Left #{n}"}.join(", ")}]"
end

def write(levels=$levels)
  levels.each do |level, s|
    File.open level, 'w' do |file|
      file.write(compile(parse_string s))
    end
  end
end

write
