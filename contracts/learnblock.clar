;; BlockLearnAdemy Course Management Contract

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-insufficient-shares (err u103))
(define-constant err-transfer-failed (err u104))
(define-constant err-invalid-input (err u105))

;; Data Variables
(define-data-var next-course-id uint u1)

;; Define the course struct
(define-map courses
  { course-id: uint }
  {
    title: (string-utf8 100),
    description: (string-utf8 500),
    instructor: principal,
    price: uint,
    total-shares: uint,
    available-shares: uint
  }
)

;; Define ownership mapping
(define-map course-ownership
  { course-id: uint, owner: principal }
  { shares: uint }
)

;; Read-only functions

(define-read-only (get-course (course-id uint))
  (map-get? courses { course-id: course-id })
)

(define-read-only (get-course-ownership (course-id uint) (owner principal))
  (default-to { shares: u0 } (map-get? course-ownership { course-id: course-id, owner: owner }))
)

;; Private functions for input validation

(define-private (validate-course-input (new-title (string-utf8 100)) (new-description (string-utf8 500)) (new-price uint) (new-total-shares uint))
  (and 
    (> (len new-title) u0)
    (> (len new-description) u0)
    (> new-price u0)
    (> new-total-shares u0)
  )
)

(define-private (validate-course-id (course-id uint))
  (is-some (map-get? courses { course-id: course-id }))
)

;; Public functions

(define-public (create-course (new-title (string-utf8 100)) (new-description (string-utf8 500)) (new-price uint) (new-total-shares uint))
  (let
    (
      (course-id (var-get next-course-id))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (is-none (map-get? courses { course-id: course-id })) err-already-exists)
    (asserts! (validate-course-input new-title new-description new-price new-total-shares) err-invalid-input)
    (map-set courses
      { course-id: course-id }
      {
        title: new-title,
        description: new-description,
        instructor: tx-sender,
        price: new-price,
        total-shares: new-total-shares,
        available-shares: new-total-shares
      }
    )
    (map-set course-ownership
      { course-id: course-id, owner: tx-sender }
      { shares: new-total-shares }
    )
    (var-set next-course-id (+ course-id u1))
    (ok course-id)
  )
)

(define-public (update-course (course-id uint) (new-title (string-utf8 100)) (new-description (string-utf8 500)) (new-price uint))
  (let
    (
      (course (unwrap! (get-course course-id) err-not-found))
    )
    (asserts! (validate-course-id course-id) err-not-found)
    (asserts! (is-eq (get instructor course) tx-sender) err-owner-only)
    (asserts! (validate-course-input new-title new-description new-price (get total-shares course)) err-invalid-input)
    (ok (map-set courses
      { course-id: course-id }
      (merge course { 
        title: new-title, 
        description: new-description, 
        price: new-price 
      })
    ))
  )
)

(define-public (buy-course-shares (course-id uint) (shares uint))
  (let
    (
      (course (unwrap! (get-course course-id) err-not-found))
      (buyer tx-sender)
      (instructor (get instructor course))
      (price-per-share (/ (get price course) (get total-shares course)))
      (total-cost (* price-per-share shares))
      (current-shares (get shares (get-course-ownership course-id buyer)))
    )
    (asserts! (validate-course-id course-id) err-not-found)
    (asserts! (> shares u0) err-invalid-input)
    (asserts! (<= shares (get available-shares course)) err-insufficient-shares)
    (try! (stx-transfer? total-cost buyer instructor))
    (map-set courses
      { course-id: course-id }
      (merge course { available-shares: (- (get available-shares course) shares) })
    )
    (map-set course-ownership
      { course-id: course-id, owner: buyer }
      { shares: (+ current-shares shares) }
    )
    (ok true)
  )
)

(define-public (transfer-course-shares (course-id uint) (to principal) (shares uint))
  (let
    (
      (sender tx-sender)
      (sender-shares (get shares (get-course-ownership course-id sender)))
      (recipient-shares (get shares (get-course-ownership course-id to)))
    )
    (asserts! (validate-course-id course-id) err-not-found)
    (asserts! (> shares u0) err-invalid-input)
    (asserts! (not (is-eq to sender)) err-invalid-input)
    (asserts! (>= sender-shares shares) err-insufficient-shares)
    (map-set course-ownership
      { course-id: course-id, owner: sender }
      { shares: (- sender-shares shares) }
    )
    (map-set course-ownership
      { course-id: course-id, owner: to }
      { shares: (+ recipient-shares shares) }
    )
    (ok true)
  )
)
