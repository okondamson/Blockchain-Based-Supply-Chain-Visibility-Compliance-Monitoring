;; Certification Management Contract
;; Manages supply chain certifications

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u500))
(define-constant ERR_CERTIFICATE_NOT_FOUND (err u501))
(define-constant ERR_CERTIFICATE_EXPIRED (err u502))
(define-constant ERR_CERTIFICATE_REVOKED (err u503))
(define-constant ERR_INVALID_STATUS (err u504))

;; Data Variables
(define-data-var next-certificate-id uint u1)

;; Data Maps
(define-map certificates
  { certificate-id: uint }
  {
    entity-id: (string-ascii 100),
    standard-id: uint,
    certificate-type: (string-ascii 50),
    issued-to: principal,
    issued-by: principal,
    issued-at: uint,
    expires-at: uint,
    status: (string-ascii 20),
    verification-hash: (string-ascii 64)
  }
)

(define-map entity-certificates
  { entity-id: (string-ascii 100) }
  { certificate-ids: (list 50 uint) }
)

(define-map certificate-verifications
  { certificate-id: uint }
  {
    verifications: (list 10 {
      verified-by: principal,
      verified-at: uint,
      verification-result: bool,
      notes: (string-ascii 200)
    })
  }
)

(define-map authorized-issuers
  { address: principal }
  { is-authorized: bool }
)

;; Initialize contract owner as authorized issuer
(map-set authorized-issuers { address: CONTRACT_OWNER } { is-authorized: true })

;; Private Functions
(define-private (is-authorized-issuer (address principal))
  (default-to false (get is-authorized (map-get? authorized-issuers { address: address })))
)

(define-private (is-valid-status (status (string-ascii 20)))
  (or (is-eq status "active") (or (is-eq status "expired") (or (is-eq status "revoked") (is-eq status "suspended"))))
)

(define-private (is-certificate-valid (certificate-id uint))
  (match (map-get? certificates { certificate-id: certificate-id })
    cert (and
      (is-eq (get status cert) "active")
      (> (get expires-at cert) block-height)
    )
    false
  )
)

;; Public Functions
(define-public (issue-certificate
  (entity-id (string-ascii 100))
  (standard-id uint)
  (certificate-type (string-ascii 50))
  (issued-to principal)
  (expires-at uint)
  (verification-hash (string-ascii 64))
)
  (let (
    (certificate-id (var-get next-certificate-id))
    (current-certificates (default-to (list) (get certificate-ids (map-get? entity-certificates { entity-id: entity-id }))))
  )
    (asserts! (is-authorized-issuer tx-sender) ERR_UNAUTHORIZED)
    (asserts! (> expires-at block-height) ERR_CERTIFICATE_EXPIRED)

    (map-set certificates
      { certificate-id: certificate-id }
      {
        entity-id: entity-id,
        standard-id: standard-id,
        certificate-type: certificate-type,
        issued-to: issued-to,
        issued-by: tx-sender,
        issued-at: block-height,
        expires-at: expires-at,
        status: "active",
        verification-hash: verification-hash
      }
    )

    (map-set entity-certificates
      { entity-id: entity-id }
      { certificate-ids: (unwrap! (as-max-len? (append current-certificates certificate-id) u50) ERR_CERTIFICATE_NOT_FOUND) }
    )

    (map-set certificate-verifications
      { certificate-id: certificate-id }
      { verifications: (list) }
    )

    (var-set next-certificate-id (+ certificate-id u1))

    (print {
      event: "certificate-issued",
      certificate-id: certificate-id,
      entity-id: entity-id,
      standard-id: standard-id,
      certificate-type: certificate-type,
      issued-to: issued-to,
      issued-by: tx-sender,
      expires-at: expires-at
    })

    (ok certificate-id)
  )
)

(define-public (revoke-certificate (certificate-id uint) (reason (string-ascii 200)))
  (let ((certificate (unwrap! (map-get? certificates { certificate-id: certificate-id }) ERR_CERTIFICATE_NOT_FOUND)))
    (asserts! (or (is-authorized-issuer tx-sender) (is-eq tx-sender (get issued-by certificate))) ERR_UNAUTHORIZED)

    (map-set certificates
      { certificate-id: certificate-id }
      (merge certificate { status: "revoked" })
    )

    (print {
      event: "certificate-revoked",
      certificate-id: certificate-id,
      revoked-by: tx-sender,
      reason: reason
    })

    (ok true)
  )
)

(define-public (verify-certificate (certificate-id uint) (verification-result bool) (notes (string-ascii 200)))
  (let (
    (certificate (unwrap! (map-get? certificates { certificate-id: certificate-id }) ERR_CERTIFICATE_NOT_FOUND))
    (current-verifications (default-to (list) (get verifications (map-get? certificate-verifications { certificate-id: certificate-id }))))
    (new-verification {
      verified-by: tx-sender,
      verified-at: block-height,
      verification-result: verification-result,
      notes: notes
    })
  )
    (asserts! (is-authorized-issuer tx-sender) ERR_UNAUTHORIZED)
    (asserts! (is-certificate-valid certificate-id) ERR_CERTIFICATE_EXPIRED)

    (map-set certificate-verifications
      { certificate-id: certificate-id }
      { verifications: (unwrap! (as-max-len? (append current-verifications new-verification) u10) ERR_CERTIFICATE_NOT_FOUND) }
    )

    (print {
      event: "certificate-verified",
      certificate-id: certificate-id,
      verified-by: tx-sender,
      verification-result: verification-result
    })

    (ok true)
  )
)

(define-public (renew-certificate (certificate-id uint) (new-expires-at uint))
  (let ((certificate (unwrap! (map-get? certificates { certificate-id: certificate-id }) ERR_CERTIFICATE_NOT_FOUND)))
    (asserts! (or (is-authorized-issuer tx-sender) (is-eq tx-sender (get issued-by certificate))) ERR_UNAUTHORIZED)
    (asserts! (> new-expires-at block-height) ERR_CERTIFICATE_EXPIRED)

    (map-set certificates
      { certificate-id: certificate-id }
      (merge certificate {
        expires-at: new-expires-at,
        status: "active"
      })
    )

    (print {
      event: "certificate-renewed",
      certificate-id: certificate-id,
      new-expires-at: new-expires-at,
      renewed-by: tx-sender
    })

    (ok true)
  )
)

(define-public (authorize-issuer (address principal))
  (begin
    (asserts! (is-authorized-issuer tx-sender) ERR_UNAUTHORIZED)
    (map-set authorized-issuers { address: address } { is-authorized: true })

    (print {
      event: "issuer-authorized",
      address: address
    })

    (ok true)
  )
)

;; Read-only Functions
(define-read-only (get-certificate (certificate-id uint))
  (map-get? certificates { certificate-id: certificate-id })
)

(define-read-only (get-entity-certificates (entity-id (string-ascii 100)))
  (map-get? entity-certificates { entity-id: entity-id })
)

(define-read-only (get-certificate-verifications (certificate-id uint))
  (map-get? certificate-verifications { certificate-id: certificate-id })
)

(define-read-only (is-certificate-active (certificate-id uint))
  (is-certificate-valid certificate-id)
)

(define-read-only (is-certificate-expired (certificate-id uint))
  (match (map-get? certificates { certificate-id: certificate-id })
    cert (< (get expires-at cert) block-height)
    true
  )
)

(define-read-only (get-next-certificate-id)
  (var-get next-certificate-id)
)

(define-read-only (check-issuer-authorization (address principal))
  (is-authorized-issuer address)
)
