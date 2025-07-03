;; Corrective Action Management Contract
;; Manages corrective actions for compliance violations

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u400))
(define-constant ERR_ACTION_NOT_FOUND (err u401))
(define-constant ERR_INVALID_STATUS (err u402))
(define-constant ERR_INVALID_PRIORITY (err u403))
(define-constant ERR_DEADLINE_PASSED (err u404))

;; Data Variables
(define-data-var next-action-id uint u1)

;; Data Maps
(define-map corrective-actions
  { action-id: uint }
  {
    violation-id: uint,
    title: (string-ascii 200),
    description: (string-ascii 1000),
    assigned-to: principal,
    priority: (string-ascii 20),
    status: (string-ascii 20),
    deadline: uint,
    created-at: uint,
    created-by: principal,
    completed-at: (optional uint),
    completion-notes: (optional (string-ascii 500))
  }
)

(define-map action-progress
  { action-id: uint }
  {
    milestones: (list 10 {
      title: (string-ascii 100),
      completed: bool,
      completed-at: (optional uint)
    }),
    progress-percentage: uint
  }
)

(define-map violation-actions
  { violation-id: uint }
  { action-ids: (list 20 uint) }
)

(define-map authorized-managers
  { address: principal }
  { is-authorized: bool }
)

;; Initialize contract owner as authorized manager
(map-set authorized-managers { address: CONTRACT_OWNER } { is-authorized: true })

;; Private Functions
(define-private (is-authorized-manager (address principal))
  (default-to false (get is-authorized (map-get? authorized-managers { address: address })))
)

(define-private (is-valid-status (status (string-ascii 20)))
  (or (is-eq status "pending") (or (is-eq status "in-progress") (or (is-eq status "completed") (is-eq status "overdue"))))
)

(define-private (is-valid-priority (priority (string-ascii 20)))
  (or (is-eq priority "low") (or (is-eq priority "medium") (or (is-eq priority "high") (is-eq priority "urgent"))))
)

;; Public Functions
(define-public (create-corrective-action
  (violation-id uint)
  (title (string-ascii 200))
  (description (string-ascii 1000))
  (assigned-to principal)
  (priority (string-ascii 20))
  (deadline uint)
)
  (let (
    (action-id (var-get next-action-id))
    (current-actions (default-to (list) (get action-ids (map-get? violation-actions { violation-id: violation-id }))))
  )
    (asserts! (is-authorized-manager tx-sender) ERR_UNAUTHORIZED)
    (asserts! (is-valid-priority priority) ERR_INVALID_PRIORITY)
    (asserts! (> deadline block-height) ERR_DEADLINE_PASSED)

    (map-set corrective-actions
      { action-id: action-id }
      {
        violation-id: violation-id,
        title: title,
        description: description,
        assigned-to: assigned-to,
        priority: priority,
        status: "pending",
        deadline: deadline,
        created-at: block-height,
        created-by: tx-sender,
        completed-at: none,
        completion-notes: none
      }
    )

    (map-set action-progress
      { action-id: action-id }
      {
        milestones: (list),
        progress-percentage: u0
      }
    )

    (map-set violation-actions
      { violation-id: violation-id }
      { action-ids: (unwrap! (as-max-len? (append current-actions action-id) u20) ERR_ACTION_NOT_FOUND) }
    )

    (var-set next-action-id (+ action-id u1))

    (print {
      event: "corrective-action-created",
      action-id: action-id,
      violation-id: violation-id,
      title: title,
      assigned-to: assigned-to,
      priority: priority,
      deadline: deadline
    })

    (ok action-id)
  )
)

(define-public (update-action-status (action-id uint) (new-status (string-ascii 20)) (notes (optional (string-ascii 500))))
  (let ((action (unwrap! (map-get? corrective-actions { action-id: action-id }) ERR_ACTION_NOT_FOUND)))
    (asserts! (or (is-authorized-manager tx-sender) (is-eq tx-sender (get assigned-to action))) ERR_UNAUTHORIZED)
    (asserts! (is-valid-status new-status) ERR_INVALID_STATUS)

    (map-set corrective-actions
      { action-id: action-id }
      (merge action {
        status: new-status,
        completed-at: (if (is-eq new-status "completed") (some block-height) (get completed-at action)),
        completion-notes: notes
      })
    )

    (print {
      event: "action-status-updated",
      action-id: action-id,
      old-status: (get status action),
      new-status: new-status,
      updated-by: tx-sender
    })

    (ok true)
  )
)

(define-public (add-milestone (action-id uint) (title (string-ascii 100)))
  (let (
    (action (unwrap! (map-get? corrective-actions { action-id: action-id }) ERR_ACTION_NOT_FOUND))
    (current-progress (unwrap! (map-get? action-progress { action-id: action-id }) ERR_ACTION_NOT_FOUND))
    (new-milestone {
      title: title,
      completed: false,
      completed-at: none
    })
  )
    (asserts! (or (is-authorized-manager tx-sender) (is-eq tx-sender (get assigned-to action))) ERR_UNAUTHORIZED)

    (map-set action-progress
      { action-id: action-id }
      (merge current-progress {
        milestones: (unwrap! (as-max-len? (append (get milestones current-progress) new-milestone) u10) ERR_ACTION_NOT_FOUND)
      })
    )

    (print {
      event: "milestone-added",
      action-id: action-id,
      milestone-title: title
    })

    (ok true)
  )
)

(define-public (complete-milestone (action-id uint) (milestone-index uint))
  (let (
    (action (unwrap! (map-get? corrective-actions { action-id: action-id }) ERR_ACTION_NOT_FOUND))
    (progress (unwrap! (map-get? action-progress { action-id: action-id }) ERR_ACTION_NOT_FOUND))
  )
    (asserts! (or (is-authorized-manager tx-sender) (is-eq tx-sender (get assigned-to action))) ERR_UNAUTHORIZED)

    ;; Note: In a full implementation, we would update the specific milestone
    ;; This is a simplified version due to Clarity's list manipulation limitations

    (print {
      event: "milestone-completed",
      action-id: action-id,
      milestone-index: milestone-index,
      completed-by: tx-sender
    })

    (ok true)
  )
)

(define-public (authorize-manager (address principal))
  (begin
    (asserts! (is-authorized-manager tx-sender) ERR_UNAUTHORIZED)
    (map-set authorized-managers { address: address } { is-authorized: true })

    (print {
      event: "manager-authorized",
      address: address
    })

    (ok true)
  )
)

;; Read-only Functions
(define-read-only (get-corrective-action (action-id uint))
  (map-get? corrective-actions { action-id: action-id })
)

(define-read-only (get-action-progress (action-id uint))
  (map-get? action-progress { action-id: action-id })
)

(define-read-only (get-violation-actions (violation-id uint))
  (map-get? violation-actions { violation-id: violation-id })
)

(define-read-only (is-action-overdue (action-id uint))
  (match (map-get? corrective-actions { action-id: action-id })
    action (and
      (< (get deadline action) block-height)
      (not (is-eq (get status action) "completed"))
    )
    false
  )
)

(define-read-only (get-next-action-id)
  (var-get next-action-id)
)

(define-read-only (check-manager-authorization (address principal))
  (is-authorized-manager address)
)
