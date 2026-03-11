## 2.8.1.0

- First version to support `network-3.0.0.0`
- Fix dead-lock in `getProtocolEntries` and make the high-level composite `get*Entries` operations atomic
- Reexport `HostAddress` and `Family` (from `network`)
- Provide `ifNameToIndex` unconditionally
- Add `NFData` instances for `ServiceEntry`, `ProtocolEntry`, `HostEntry`, and `NetworkEntry`
