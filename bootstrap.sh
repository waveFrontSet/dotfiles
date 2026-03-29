#!/usr/bin/env bash
echo "Installing Nix (if not present)..."
command -v nix >/dev/null || curl --proto "=https" --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install
echo "Installing rustup (if not present)..."
command -v rustup >/dev/null || curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y --no-modify-path
echo "Installing ghcup (if not present)..."
command -v ghcup >/dev/null || curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_ADJUST_BASHRC=0 sh
echo "Setting up SSH commit signing..."
/bin/bash -c "if [ -f ~/.ssh/id_ed25519.pub ]; then echo \"$(git config user.email) $(cat ~/.ssh/id_ed25519.pub)\" > ~/.ssh/allowed_signers && echo \"Created ~/.ssh/allowed_signers for SSH commit verification.\"; else echo \"WARNING: ~/.ssh/id_ed25519.pub not found. Generate an SSH key with: ssh-keygen -t ed25519\"; fi; echo \"REMINDER: Add your SSH key to GitHub as a signing key: gh ssh-key add ~/.ssh/id_ed25519.pub --type signing\""
