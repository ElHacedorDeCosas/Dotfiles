if status is-interactive
    # Commands to run in interactive sessions can go here
    fastfetch


end
fzf --fish | source

#Config
set -g theme_powerline_fonts no
set -g theme_nerd_fonts yes
set -g theme_color_scheme nord

#Alias
alias ran="ranger"
alias ls="lsd"
alias z="zellij"

#eval (zellij setup --generate-auto-start fish | string collect)
eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"

#Variables de entorno
set -x PATH $PATH /home/gonzaloc/.odin/
set -x PATH $PATH /home/gonzaloc/.config/emacs/bin/
#set -x PATH $PATH /home/gonzaloc/.onyx/bin/onyx
#set -x PATH $PATH /home/gonzaloc/.c3/
set -x PATH $PATH /home/gonzaloc/.ada
set -ga fish_user_paths /home/gonzaloc/.nimble/bin
set -x PATH $PATH /home/gonzaloc/go/bin/

function y
	set tmp (mktemp -t "yazi-cwd.XXXXXX")
	yazi $argv --cwd-file="$tmp"
	if set cwd (command cat -- "$tmp"); and [ -n "$cwd" ]; and [ "$cwd" != "$PWD" ]
		builtin cd -- "$cwd"
	end
	rm -f -- "$tmp"
end
#starship init fish | source
