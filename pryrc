def h(lines = 100)
  File.open('/dev/tty', 'w') do |tty|
    `grep -v -- --inf-ruby-- ~/.local/share/pry/pry_history | tail -n #{lines} | fzf --tac --bind alt-space:jump-accept --color light | tr -d '\n'`.each_char do |c|
      tty.ioctl(0x5412,c)
    end
  end
end
