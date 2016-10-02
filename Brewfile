# https://github.com/Homebrew/homebrew-bundle

# - some cask packages and App Store applications ask for password
# - see comments before package or application for postinstallation setup
# - see `brew services` on how to manage services for supported forumalae

cask_args appdir: '/Applications'

tap 'caskroom/cask'
tap 'caskroom/versions'
tap 'homebrew/bundle'
tap 'homebrew/services'
tap 'homebrew/versions'

#brew 'chromedriver'
#brew 'djview4'
#brew 'phantomjs'

brew 'elixir'
brew 'fontforge'
brew 'git'
brew 'gpg'
brew 'htop'
brew 'imagemagick'
brew 'macvim'
brew 'mas'
brew 'memcached', restart_service: :changed
brew 'mc'
brew 'mpv'
brew 'postgresql', restart_service: :changed
# it's easier to install pow manually:
# - curl get.pow.cx | sh
# - ln -s ~/dev/reenter_builder ~/.pow
#brew 'pow'
# http://tap349.github.io/rbenv/ruby/chef/capistrano/2016/03/30/rbenv/
brew 'rbenv'
brew 'rbenv-ctags'
brew 'redis', restart_service: :changed
brew 'ssh-copy-id'
brew 'the_silver_searcher'
brew 'tree'
brew 'unrar'
brew 'wget'
# - make it a login shell: chsh -s /bin/zsh
#   (all available shells are listed in /etc/shells)
# - install oh-my-zsh with plugins:
#   - https://github.com/zsh-users/zsh-autosuggestions#oh-my-zsh
#   - https://github.com/zsh-users/zsh-syntax-highlighting/blob/master/INSTALL.md#oh-my-zsh
brew 'zsh'

cask 'chefdk'
cask 'flux'
cask 'google-chrome'
cask 'iterm2'
cask 'skype'
# * system preferences:
#   - Users & Groups -> Login Items (don't hide)
# * app preferences:
#   - Menubar: Temperature only
cask 'smcfancontrol'
# open /usr/local/Caskroom/utorrent/latest/uTorrent.app
cask 'utorrent'

mas '2Do', id: 477670270
mas 'Cloud Mail.Ru', id: 893068358
mas 'Dashlane - Password Manager, Secure Digital Wallet', id: 552383089
mas 'Evernote', id: 406056744
# * system preferences:
#   - Security & Privacy -> Privacy -> Accessibility
#   - Users & Groups -> Login Items (hide)
# * app preferences:
#   - Window Mover:
#     - Move: <S-M> + 1 finger
#     - Resize: <S-M> + 2 finger
#   - Layouts:
#     - Maximize: <S-M-CR>
#   - Preferences:
#     - don't show icon in Dock
mas 'Flexiglass', id: 426410278
mas 'Marked 2', id: 890031187
mas 'Monosnap', id: 540348655
mas 'Pomodoro Timer', id: 872515009
mas 'VPNAutoConnect', id: 532510878
mas 'Xcode', id: 497799835
mas 'Sip', id: 507257563
