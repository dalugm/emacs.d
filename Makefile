# Copyright (C) 2021 dalu

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

-include lib/borg/borg.mk

bootstrap-borg:
	@git submodule--helper clone --name borg --path lib/borg \
	--url https://github.com/emacscollective/borg.git
	@cd lib/borg; git symbolic-ref HEAD refs/heads/master
	@cd lib/borg; git reset --hard HEAD

bootstrap-shallow:
	@printf "\n=== Running 'git submodule init' ===\n\n"
	@git submodule init
	@printf "\n=== Running 'borg-shallow.sh' ===\n"
	@./borg-shallow.sh
	@printf "\n=== Running 'make build' ===\n\n"
	@make build

helpall::
	$(info Test and fix targets)
	$(info --------------------)
	$(info make bootstrap-shallow	= shallow bootstrap configurations)
	$(info make codespell-dry	= run codespell, dry run)
	$(info make codespell-fix	= run codespell, write fixes)
	$(info )

codespell-dry:
	@cd lib; codespell \
	  --ignore-words ~/.emacs.d/etc/codespell/ignore-words \
	  --exclude-file ~/.emacs.d/etc/codespell/ignore-lines \
	  --skip $(shell sed '/^\s*$$/d;/^\s*#.*$$/d;s/#.*//;s/\s//g' \
	  ~/.emacs.d/etc/codespell/ignore-files | tr "\\n" ",")

codespell-fix:
	@cd lib; codespell --write-changes \
	  --ignore-words ~/.emacs.d/etc/codespell/ignore-words \
	  --exclude-file ~/.emacs.d/etc/codespell/ignore-lines \
	  --skip $(shell sed '/^\s*$$/d;/^\s*#.*$$/d;s/#.*//;s/\s//g' \
	  ~/.emacs.d/etc/codespell/ignore-files | tr "\\n" ",")
