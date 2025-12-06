//! Shortcut keys

// Imports
use {
	crate::{ViewMode, dir_reader::SortOrderKind, util::hashmap_serialize_sorted},
	egui::Modifiers,
	serde::de::Error as DeserializeError,
	std::collections::HashMap,
	strum::VariantArray,
};

/// Shortcut keys
// TODO: Allow modifiers here.
#[derive(Clone, Debug)]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct Shortcuts {
	pub prev: ShortcutKey,
	pub next: ShortcutKey,

	pub pan_up:   ShortcutKey,
	pub pan_down: ShortcutKey,

	pub fit_width_if_default: ShortcutKey,

	pub first: ShortcutKey,
	pub last:  ShortcutKey,

	pub fullscreen:              ShortcutKey,
	pub exit_fullscreen_or_quit: ShortcutKey,

	pub toggle_pause: ShortcutKey,
	pub quit:         ShortcutKey,

	pub toggle_display_mode: ShortcutKey,

	#[serde(serialize_with = "hashmap_serialize_sorted")]
	pub sort: HashMap<SortOrderKind, ShortcutKey>,

	#[serde(serialize_with = "hashmap_serialize_sorted")]
	pub view_modes: HashMap<ViewMode, ShortcutKey>,
}

impl Default for Shortcuts {
	fn default() -> Self {
		Self {
			prev:                    egui::Key::ArrowLeft.into(),
			next:                    egui::Key::ArrowRight.into(),
			pan_up:                  egui::Key::ArrowUp.into(),
			pan_down:                egui::Key::ArrowDown.into(),
			fit_width_if_default:    egui::Key::ArrowUp.into(),
			first:                   egui::Key::Home.into(),
			last:                    egui::Key::End.into(),
			fullscreen:              egui::Key::F.into(),
			exit_fullscreen_or_quit: egui::Key::Escape.into(),
			toggle_pause:            egui::Key::Space.into(),
			quit:                    egui::Key::Q.into(),
			toggle_display_mode:     egui::Key::Backspace.into(),
			sort:                    SortOrderKind::VARIANTS
				.iter()
				.map(|&kind| {
					let key = match kind {
						SortOrderKind::FileName => egui::Key::F1,
						SortOrderKind::ModificationDate => egui::Key::F2,
						SortOrderKind::Size => egui::Key::F3,
					};

					(kind, key.into())
				})
				.collect(),
			view_modes:              ViewMode::VARIANTS
				.iter()
				.map(|&kind| {
					let key = match kind {
						ViewMode::FitWindow => egui::Key::Num1,
						ViewMode::FitWidth => egui::Key::Num2,
						ViewMode::ActualSize => egui::Key::Num3,
					};

					(kind, key.into())
				})
				.collect(),
		}
	}
}

/// Shortcut key
#[derive(Clone, Copy, Debug)]
pub struct ShortcutKey {
	pub modifiers: egui::Modifiers,
	pub key:       egui::Key,
}

impl ShortcutKey {
	/// A sentinel value meaning that no key was bound.
	// TODO: We shouldn't be using an existing key despite
	//       the user never using this one.
	pub const UNBOUND: Self = Self {
		modifiers: egui::Modifiers {
			alt:     true,
			ctrl:    true,
			shift:   true,
			mac_cmd: true,
			command: true,
		},
		key:       egui::Key::Escape,
	};
}

impl From<egui::Key> for ShortcutKey {
	fn from(key: egui::Key) -> Self {
		Self {
			modifiers: Modifiers::NONE,
			key,
		}
	}
}

/// Extension trait for `egui::Input` using the shortcut keys
#[extend::ext]
pub impl egui::InputState {
	fn consume_shortcut_key(&mut self, key: ShortcutKey) -> bool {
		self.consume_key(key.modifiers, key.key)
	}
}

#[derive(Clone, Debug)]
#[derive(serde::Serialize, serde::Deserialize)]
#[serde(untagged)]
enum ShortcutKeyRepr {
	OnlyKey(egui::Key),
	WithModifiers(Vec<ShortcutKeyReprComponent>),
}

#[derive(Clone, Copy, Debug)]
#[derive(serde::Serialize, serde::Deserialize)]
enum ShortcutKeyReprComponent {
	#[serde(rename = "alt")]
	Alt,
	#[serde(rename = "ctrl")]
	Ctrl,
	#[serde(rename = "shift")]
	Shift,
	#[serde(rename = "mac_cmd")]
	MacCmd,
	#[serde(rename = "command")]
	Command,

	#[serde(untagged)]
	Key(egui::Key),
}

impl serde::Serialize for ShortcutKey {
	fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
		S: serde::Serializer,
	{
		// If we have no modifiers, just serialize the key itself
		if self.modifiers.is_none() {
			return ShortcutKeyRepr::OnlyKey(self.key).serialize(serializer);
		}

		// Otherwise, serialize the key with all the modifiers in an array
		let mut cmpts = vec![];
		cmpts.push(ShortcutKeyReprComponent::Key(self.key));
		if self.modifiers.alt {
			cmpts.push(ShortcutKeyReprComponent::Alt);
		}
		if self.modifiers.ctrl {
			cmpts.push(ShortcutKeyReprComponent::Ctrl);
		}
		if self.modifiers.shift {
			cmpts.push(ShortcutKeyReprComponent::Shift);
		}
		if self.modifiers.mac_cmd {
			cmpts.push(ShortcutKeyReprComponent::MacCmd);
		}
		if self.modifiers.command {
			cmpts.push(ShortcutKeyReprComponent::Command);
		}

		ShortcutKeyRepr::WithModifiers(cmpts).serialize(serializer)
	}
}

impl<'de> serde::Deserialize<'de> for ShortcutKey {
	fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
	where
		D: serde::Deserializer<'de>,
	{
		let repr = ShortcutKeyRepr::deserialize(deserializer)?;
		let key = match repr {
			ShortcutKeyRepr::OnlyKey(key) => Self {
				modifiers: Modifiers::NONE,
				key,
			},
			ShortcutKeyRepr::WithModifiers(cmpts) => {
				let mut modifiers = egui::Modifiers::NONE;
				let mut cur_key = None;

				for cmpt in cmpts {
					match cmpt {
						ShortcutKeyReprComponent::Alt => modifiers.alt = true,
						ShortcutKeyReprComponent::Ctrl => modifiers.ctrl = true,
						ShortcutKeyReprComponent::Shift => modifiers.shift = true,
						ShortcutKeyReprComponent::MacCmd => modifiers.mac_cmd = true,
						ShortcutKeyReprComponent::Command => modifiers.command = true,
						ShortcutKeyReprComponent::Key(key) => match cur_key {
							Some(_) => return Err(D::Error::duplicate_field("key")),
							None => cur_key = Some(key),
						},
					}
				}

				let key = cur_key.ok_or_else(|| D::Error::missing_field("key"))?;

				Self { modifiers, key }
			},
		};

		Ok(key)
	}
}
