;; CryptVaultというdatatypeを作る.
;; イメージ的にはクラス定義とインスタンス作成みたいな.
(deftype CryptVault [filename keystore password])
(def vault (->CryptVault "vault-file" "keystore" "toomanysecrets"))
;; user=> (.filename vault)
;; "vault-file"

;; Datatypeは, protocol or interfaceで定義したメソッドのみimplement可能.
;; なのでprotocolを用意する
(defprotocol Vault
  (init-vault [vault])
  (vault-output-stream [vault])
  (vault-input-stream [vault]))

;; 複数のprotocolに対するmethodを同じ場所で定義できるのがわかる.

