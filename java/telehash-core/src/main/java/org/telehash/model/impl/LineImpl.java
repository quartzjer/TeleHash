/**
 * <copyright>
 * </copyright>
 *
 * $Id$
 */
package org.telehash.model.impl;

import java.net.InetSocketAddress;
import java.security.SecureRandom;
import java.util.Collection;

import org.apache.mina.core.session.IoSession;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;
import org.eclipse.emf.ecore.util.EDataTypeUniqueEList;
import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.InternalEList;
import org.telehash.Hash;
import org.telehash.SwitchHandler;
import org.telehash.model.Line;
import org.telehash.model.TapRule;
import org.telehash.model.TelehashPackage;
import org.telehash.model.Telex;

import com.google.common.base.Objects;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Line</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.telehash.model.impl.LineImpl#getAddress <em>Address</em>}</li>
 *   <li>{@link org.telehash.model.impl.LineImpl#getEnd <em>End</em>}</li>
 *   <li>{@link org.telehash.model.impl.LineImpl#getNeighbors <em>Neighbors</em>}</li>
 *   <li>{@link org.telehash.model.impl.LineImpl#getRingIn <em>Ring In</em>}</li>
 *   <li>{@link org.telehash.model.impl.LineImpl#getRingOut <em>Ring Out</em>}</li>
 *   <li>{@link org.telehash.model.impl.LineImpl#getInit <em>Init</em>}</li>
 *   <li>{@link org.telehash.model.impl.LineImpl#getSeenAt <em>Seen At</em>}</li>
 *   <li>{@link org.telehash.model.impl.LineImpl#getSentAt <em>Sent At</em>}</li>
 *   <li>{@link org.telehash.model.impl.LineImpl#getLineAt <em>Line At</em>}</li>
 *   <li>{@link org.telehash.model.impl.LineImpl#getTapLastAt <em>Tap Last At</em>}</li>
 *   <li>{@link org.telehash.model.impl.LineImpl#getBr <em>Br</em>}</li>
 *   <li>{@link org.telehash.model.impl.LineImpl#getBrIn <em>Br In</em>}</li>
 *   <li>{@link org.telehash.model.impl.LineImpl#getBrOut <em>Br Out</em>}</li>
 *   <li>{@link org.telehash.model.impl.LineImpl#getBsent <em>Bsent</em>}</li>
 *   <li>{@link org.telehash.model.impl.LineImpl#getLineId <em>Line Id</em>}</li>
 *   <li>{@link org.telehash.model.impl.LineImpl#isVisible <em>Visible</em>}</li>
 *   <li>{@link org.telehash.model.impl.LineImpl#isAdvertised <em>Advertised</em>}</li>
 *   <li>{@link org.telehash.model.impl.LineImpl#getRules <em>Rules</em>}</li>
 *   <li>{@link org.telehash.model.impl.LineImpl#getSession <em>Session</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class LineImpl extends EObjectImpl implements Line {
	/**
	 * The default value of the '{@link #getAddress() <em>Address</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getAddress()
	 * @generated
	 * @ordered
	 */
	protected static final InetSocketAddress ADDRESS_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getAddress() <em>Address</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getAddress()
	 * @generated
	 * @ordered
	 */
	protected InetSocketAddress address = ADDRESS_EDEFAULT;

	/**
	 * The default value of the '{@link #getEnd() <em>End</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getEnd()
	 * @generated
	 * @ordered
	 */
	protected static final Hash END_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getEnd() <em>End</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getEnd()
	 * @generated
	 * @ordered
	 */
	protected Hash end = END_EDEFAULT;

	/**
	 * The cached value of the '{@link #getNeighbors() <em>Neighbors</em>}' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getNeighbors()
	 * @generated
	 * @ordered
	 */
	protected EList<Hash> neighbors;

	/**
	 * The default value of the '{@link #getRingIn() <em>Ring In</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getRingIn()
	 * @generated
	 * @ordered
	 */
	protected static final int RING_IN_EDEFAULT = 0;

	/**
	 * The cached value of the '{@link #getRingIn() <em>Ring In</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getRingIn()
	 * @generated
	 * @ordered
	 */
	protected int ringIn = RING_IN_EDEFAULT;

	/**
	 * This is true if the Ring In attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean ringInESet;

	/**
	 * The default value of the '{@link #getRingOut() <em>Ring Out</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getRingOut()
	 * @generated
	 * @ordered
	 */
	protected static final int RING_OUT_EDEFAULT = 0;

	/**
	 * The cached value of the '{@link #getRingOut() <em>Ring Out</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getRingOut()
	 * @generated
	 * @ordered
	 */
	protected int ringOut = RING_OUT_EDEFAULT;

	/**
	 * This is true if the Ring Out attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean ringOutESet;

	/**
	 * The default value of the '{@link #getInit() <em>Init</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getInit()
	 * @generated
	 * @ordered
	 */
	protected static final long INIT_EDEFAULT = 0L;

	/**
	 * The cached value of the '{@link #getInit() <em>Init</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getInit()
	 * @generated
	 * @ordered
	 */
	protected long init = INIT_EDEFAULT;

	/**
	 * This is true if the Init attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean initESet;

	/**
	 * The default value of the '{@link #getSeenAt() <em>Seen At</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getSeenAt()
	 * @generated
	 * @ordered
	 */
	protected static final long SEEN_AT_EDEFAULT = 0L;

	/**
	 * The cached value of the '{@link #getSeenAt() <em>Seen At</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getSeenAt()
	 * @generated
	 * @ordered
	 */
	protected long seenAt = SEEN_AT_EDEFAULT;

	/**
	 * This is true if the Seen At attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean seenAtESet;

	/**
	 * The default value of the '{@link #getSentAt() <em>Sent At</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getSentAt()
	 * @generated
	 * @ordered
	 */
	protected static final long SENT_AT_EDEFAULT = 0L;

	/**
	 * The cached value of the '{@link #getSentAt() <em>Sent At</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getSentAt()
	 * @generated
	 * @ordered
	 */
	protected long sentAt = SENT_AT_EDEFAULT;

	/**
	 * This is true if the Sent At attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean sentAtESet;

	/**
	 * The default value of the '{@link #getLineAt() <em>Line At</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getLineAt()
	 * @generated
	 * @ordered
	 */
	protected static final long LINE_AT_EDEFAULT = 0L;

	/**
	 * The cached value of the '{@link #getLineAt() <em>Line At</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getLineAt()
	 * @generated
	 * @ordered
	 */
	protected long lineAt = LINE_AT_EDEFAULT;

	/**
	 * This is true if the Line At attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean lineAtESet;

	/**
	 * The default value of the '{@link #getTapLastAt() <em>Tap Last At</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getTapLastAt()
	 * @generated
	 * @ordered
	 */
	protected static final int TAP_LAST_AT_EDEFAULT = 0;

	/**
	 * The cached value of the '{@link #getTapLastAt() <em>Tap Last At</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getTapLastAt()
	 * @generated
	 * @ordered
	 */
	protected int tapLastAt = TAP_LAST_AT_EDEFAULT;

	/**
	 * This is true if the Tap Last At attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean tapLastAtESet;

	/**
	 * The default value of the '{@link #getBr() <em>Br</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getBr()
	 * @generated
	 * @ordered
	 */
	protected static final int BR_EDEFAULT = 0;

	/**
	 * The cached value of the '{@link #getBr() <em>Br</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getBr()
	 * @generated
	 * @ordered
	 */
	protected int br = BR_EDEFAULT;

	/**
	 * The default value of the '{@link #getBrIn() <em>Br In</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getBrIn()
	 * @generated
	 * @ordered
	 */
	protected static final int BR_IN_EDEFAULT = 0;

	/**
	 * The cached value of the '{@link #getBrIn() <em>Br In</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getBrIn()
	 * @generated
	 * @ordered
	 */
	protected int brIn = BR_IN_EDEFAULT;

	/**
	 * The default value of the '{@link #getBrOut() <em>Br Out</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getBrOut()
	 * @generated
	 * @ordered
	 */
	protected static final int BR_OUT_EDEFAULT = 0;

	/**
	 * The cached value of the '{@link #getBrOut() <em>Br Out</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getBrOut()
	 * @generated
	 * @ordered
	 */
	protected int brOut = BR_OUT_EDEFAULT;

	/**
	 * The default value of the '{@link #getBsent() <em>Bsent</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getBsent()
	 * @generated
	 * @ordered
	 */
	protected static final int BSENT_EDEFAULT = 0;

	/**
	 * The cached value of the '{@link #getBsent() <em>Bsent</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getBsent()
	 * @generated
	 * @ordered
	 */
	protected int bsent = BSENT_EDEFAULT;

	/**
	 * The default value of the '{@link #getLineId() <em>Line Id</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getLineId()
	 * @generated
	 * @ordered
	 */
	protected static final int LINE_ID_EDEFAULT = 0;

	/**
	 * The cached value of the '{@link #getLineId() <em>Line Id</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getLineId()
	 * @generated
	 * @ordered
	 */
	protected int lineId = LINE_ID_EDEFAULT;

	/**
	 * This is true if the Line Id attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean lineIdESet;

	/**
	 * The default value of the '{@link #isVisible() <em>Visible</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isVisible()
	 * @generated
	 * @ordered
	 */
	protected static final boolean VISIBLE_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isVisible() <em>Visible</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isVisible()
	 * @generated
	 * @ordered
	 */
	protected boolean visible = VISIBLE_EDEFAULT;

	/**
	 * The default value of the '{@link #isAdvertised() <em>Advertised</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isAdvertised()
	 * @generated
	 * @ordered
	 */
	protected static final boolean ADVERTISED_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isAdvertised() <em>Advertised</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isAdvertised()
	 * @generated
	 * @ordered
	 */
	protected boolean advertised = ADVERTISED_EDEFAULT;

	/**
	 * The cached value of the '{@link #getRules() <em>Rules</em>}' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getRules()
	 * @generated
	 * @ordered
	 */
	protected EList<TapRule> rules;

	/**
	 * The default value of the '{@link #getSession() <em>Session</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getSession()
	 * @generated
	 * @ordered
	 */
	protected static final IoSession SESSION_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getSession() <em>Session</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getSession()
	 * @generated
	 * @ordered
	 */
	protected IoSession session = SESSION_EDEFAULT;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated NOT
	 */
	protected LineImpl() {
		super();
		setInit(SwitchHandler.time());
		setRingOut(new SecureRandom().nextInt(32767) + 1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return TelehashPackage.Literals.LINE;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public InetSocketAddress getAddress() {
		return address;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Line withAddress(InetSocketAddress value) {
		setAddress(value);
		return this;
	}

	public void setAddress(InetSocketAddress newAddress) {
		InetSocketAddress oldAddress = address;
		address = newAddress;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					TelehashPackage.LINE__ADDRESS, oldAddress, address));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Hash getEnd() {
		return end;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Line withEnd(Hash value) {
		setEnd(value);
		return this;
	}

	public void setEnd(Hash newEnd) {
		Hash oldEnd = end;
		end = newEnd;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					TelehashPackage.LINE__END, oldEnd, end));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList<Hash> getNeighbors() {
		if (neighbors == null) {
			neighbors = new EDataTypeUniqueEList<Hash>(Hash.class, this,
					TelehashPackage.LINE__NEIGHBORS);
		}
		return neighbors;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public int getRingIn() {
		return ringIn;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Line withRingIn(int value) {
		setRingIn(value);
		return this;
	}

	public void setRingIn(int newRingIn) {
		int oldRingIn = ringIn;
		ringIn = newRingIn;
		boolean oldRingInESet = ringInESet;
		ringInESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					TelehashPackage.LINE__RING_IN, oldRingIn, ringIn,
					!oldRingInESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetRingIn() {
		int oldRingIn = ringIn;
		boolean oldRingInESet = ringInESet;
		ringIn = RING_IN_EDEFAULT;
		ringInESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET,
					TelehashPackage.LINE__RING_IN, oldRingIn, RING_IN_EDEFAULT,
					oldRingInESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetRingIn() {
		return ringInESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public int getRingOut() {
		return ringOut;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Line withRingOut(int value) {
		setRingOut(value);
		return this;
	}

	public void setRingOut(int newRingOut) {
		int oldRingOut = ringOut;
		ringOut = newRingOut;
		boolean oldRingOutESet = ringOutESet;
		ringOutESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					TelehashPackage.LINE__RING_OUT, oldRingOut, ringOut,
					!oldRingOutESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetRingOut() {
		int oldRingOut = ringOut;
		boolean oldRingOutESet = ringOutESet;
		ringOut = RING_OUT_EDEFAULT;
		ringOutESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET,
					TelehashPackage.LINE__RING_OUT, oldRingOut,
					RING_OUT_EDEFAULT, oldRingOutESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetRingOut() {
		return ringOutESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public long getInit() {
		return init;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Line withInit(long value) {
		setInit(value);
		return this;
	}

	public void setInit(long newInit) {
		long oldInit = init;
		init = newInit;
		boolean oldInitESet = initESet;
		initESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					TelehashPackage.LINE__INIT, oldInit, init, !oldInitESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetInit() {
		long oldInit = init;
		boolean oldInitESet = initESet;
		init = INIT_EDEFAULT;
		initESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET,
					TelehashPackage.LINE__INIT, oldInit, INIT_EDEFAULT,
					oldInitESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetInit() {
		return initESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public long getSeenAt() {
		return seenAt;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Line withSeenAt(long value) {
		setSeenAt(value);
		return this;
	}

	public void setSeenAt(long newSeenAt) {
		long oldSeenAt = seenAt;
		seenAt = newSeenAt;
		boolean oldSeenAtESet = seenAtESet;
		seenAtESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					TelehashPackage.LINE__SEEN_AT, oldSeenAt, seenAt,
					!oldSeenAtESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetSeenAt() {
		long oldSeenAt = seenAt;
		boolean oldSeenAtESet = seenAtESet;
		seenAt = SEEN_AT_EDEFAULT;
		seenAtESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET,
					TelehashPackage.LINE__SEEN_AT, oldSeenAt, SEEN_AT_EDEFAULT,
					oldSeenAtESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetSeenAt() {
		return seenAtESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public long getSentAt() {
		return sentAt;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Line withSentAt(long value) {
		setSentAt(value);
		return this;
	}

	public void setSentAt(long newSentAt) {
		long oldSentAt = sentAt;
		sentAt = newSentAt;
		boolean oldSentAtESet = sentAtESet;
		sentAtESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					TelehashPackage.LINE__SENT_AT, oldSentAt, sentAt,
					!oldSentAtESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetSentAt() {
		long oldSentAt = sentAt;
		boolean oldSentAtESet = sentAtESet;
		sentAt = SENT_AT_EDEFAULT;
		sentAtESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET,
					TelehashPackage.LINE__SENT_AT, oldSentAt, SENT_AT_EDEFAULT,
					oldSentAtESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetSentAt() {
		return sentAtESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public long getLineAt() {
		return lineAt;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Line withLineAt(long value) {
		setLineAt(value);
		return this;
	}

	public void setLineAt(long newLineAt) {
		long oldLineAt = lineAt;
		lineAt = newLineAt;
		boolean oldLineAtESet = lineAtESet;
		lineAtESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					TelehashPackage.LINE__LINE_AT, oldLineAt, lineAt,
					!oldLineAtESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetLineAt() {
		long oldLineAt = lineAt;
		boolean oldLineAtESet = lineAtESet;
		lineAt = LINE_AT_EDEFAULT;
		lineAtESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET,
					TelehashPackage.LINE__LINE_AT, oldLineAt, LINE_AT_EDEFAULT,
					oldLineAtESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetLineAt() {
		return lineAtESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public int getTapLastAt() {
		return tapLastAt;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Line withTapLastAt(int value) {
		setTapLastAt(value);
		return this;
	}

	public void setTapLastAt(int newTapLastAt) {
		int oldTapLastAt = tapLastAt;
		tapLastAt = newTapLastAt;
		boolean oldTapLastAtESet = tapLastAtESet;
		tapLastAtESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					TelehashPackage.LINE__TAP_LAST_AT, oldTapLastAt, tapLastAt,
					!oldTapLastAtESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetTapLastAt() {
		int oldTapLastAt = tapLastAt;
		boolean oldTapLastAtESet = tapLastAtESet;
		tapLastAt = TAP_LAST_AT_EDEFAULT;
		tapLastAtESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET,
					TelehashPackage.LINE__TAP_LAST_AT, oldTapLastAt,
					TAP_LAST_AT_EDEFAULT, oldTapLastAtESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetTapLastAt() {
		return tapLastAtESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public int getBr() {
		return br;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Line withBr(int value) {
		setBr(value);
		return this;
	}

	public void setBr(int newBr) {
		int oldBr = br;
		br = newBr;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					TelehashPackage.LINE__BR, oldBr, br));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public int getBrIn() {
		return brIn;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Line withBrIn(int value) {
		setBrIn(value);
		return this;
	}

	public void setBrIn(int newBrIn) {
		int oldBrIn = brIn;
		brIn = newBrIn;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					TelehashPackage.LINE__BR_IN, oldBrIn, brIn));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public int getBrOut() {
		return brOut;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Line withBrOut(int value) {
		setBrOut(value);
		return this;
	}

	public void setBrOut(int newBrOut) {
		int oldBrOut = brOut;
		brOut = newBrOut;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					TelehashPackage.LINE__BR_OUT, oldBrOut, brOut));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public int getBsent() {
		return bsent;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Line withBsent(int value) {
		setBsent(value);
		return this;
	}

	public void setBsent(int newBsent) {
		int oldBsent = bsent;
		bsent = newBsent;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					TelehashPackage.LINE__BSENT, oldBsent, bsent));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public int getLineId() {
		return lineId;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Line withLineId(int value) {
		setLineId(value);
		return this;
	}

	public void setLineId(int newLineId) {
		int oldLineId = lineId;
		lineId = newLineId;
		boolean oldLineIdESet = lineIdESet;
		lineIdESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					TelehashPackage.LINE__LINE_ID, oldLineId, lineId,
					!oldLineIdESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetLineId() {
		int oldLineId = lineId;
		boolean oldLineIdESet = lineIdESet;
		lineId = LINE_ID_EDEFAULT;
		lineIdESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET,
					TelehashPackage.LINE__LINE_ID, oldLineId, LINE_ID_EDEFAULT,
					oldLineIdESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetLineId() {
		return lineIdESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isVisible() {
		return visible;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Line withVisible(boolean value) {
		setVisible(value);
		return this;
	}

	public void setVisible(boolean newVisible) {
		boolean oldVisible = visible;
		visible = newVisible;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					TelehashPackage.LINE__VISIBLE, oldVisible, visible));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isAdvertised() {
		return advertised;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Line withAdvertised(boolean value) {
		setAdvertised(value);
		return this;
	}

	public void setAdvertised(boolean newAdvertised) {
		boolean oldAdvertised = advertised;
		advertised = newAdvertised;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					TelehashPackage.LINE__ADVERTISED, oldAdvertised, advertised));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList<TapRule> getRules() {
		if (rules == null) {
			rules = new EObjectContainmentEList.Unsettable<TapRule>(
					TapRule.class, this, TelehashPackage.LINE__RULES);
		}
		return rules;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetRules() {
		if (rules != null)
			((InternalEList.Unsettable<?>) rules).unset();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetRules() {
		return rules != null && ((InternalEList.Unsettable<?>) rules).isSet();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public IoSession getSession() {
		return session;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Line withSession(IoSession value) {
		setSession(value);
		return this;
	}

	public void setSession(IoSession newSession) {
		IoSession oldSession = session;
		session = newSession;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					TelehashPackage.LINE__SESSION, oldSession, session));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated NOT
	 */
	public boolean isRulesMatch(Telex telex) {
		for (TapRule tapRule : getRules()) {
			if (!isRuleMatch(tapRule, telex)) {
				return false;
			}
		}
		return true;
	}
	
	private boolean isRuleMatch(TapRule tapRule, Telex telex) {
		for (String hasKey : tapRule.getHas()) {
			if (telex.get(hasKey) == null) {
				return false;
			}
		}
		for (String isField : tapRule.getIs().getFieldNames()) {
			if (!Objects.equal(tapRule.get(isField), telex.get(isField))) {
				return false;
			}
		}
		return true;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd,
			int featureID, NotificationChain msgs) {
		switch (featureID) {
		case TelehashPackage.LINE__RULES:
			return ((InternalEList<?>) getRules()).basicRemove(otherEnd, msgs);
		}
		return super.eInverseRemove(otherEnd, featureID, msgs);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
		case TelehashPackage.LINE__ADDRESS:
			return getAddress();
		case TelehashPackage.LINE__END:
			return getEnd();
		case TelehashPackage.LINE__NEIGHBORS:
			return getNeighbors();
		case TelehashPackage.LINE__RING_IN:
			return getRingIn();
		case TelehashPackage.LINE__RING_OUT:
			return getRingOut();
		case TelehashPackage.LINE__INIT:
			return getInit();
		case TelehashPackage.LINE__SEEN_AT:
			return getSeenAt();
		case TelehashPackage.LINE__SENT_AT:
			return getSentAt();
		case TelehashPackage.LINE__LINE_AT:
			return getLineAt();
		case TelehashPackage.LINE__TAP_LAST_AT:
			return getTapLastAt();
		case TelehashPackage.LINE__BR:
			return getBr();
		case TelehashPackage.LINE__BR_IN:
			return getBrIn();
		case TelehashPackage.LINE__BR_OUT:
			return getBrOut();
		case TelehashPackage.LINE__BSENT:
			return getBsent();
		case TelehashPackage.LINE__LINE_ID:
			return getLineId();
		case TelehashPackage.LINE__VISIBLE:
			return isVisible();
		case TelehashPackage.LINE__ADVERTISED:
			return isAdvertised();
		case TelehashPackage.LINE__RULES:
			return getRules();
		case TelehashPackage.LINE__SESSION:
			return getSession();
		}
		return super.eGet(featureID, resolve, coreType);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@SuppressWarnings("unchecked")
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
		case TelehashPackage.LINE__ADDRESS:
			setAddress((InetSocketAddress) newValue);
			return;
		case TelehashPackage.LINE__END:
			setEnd((Hash) newValue);
			return;
		case TelehashPackage.LINE__NEIGHBORS:
			getNeighbors().clear();
			getNeighbors().addAll((Collection<? extends Hash>) newValue);
			return;
		case TelehashPackage.LINE__RING_IN:
			setRingIn((Integer) newValue);
			return;
		case TelehashPackage.LINE__RING_OUT:
			setRingOut((Integer) newValue);
			return;
		case TelehashPackage.LINE__INIT:
			setInit((Long) newValue);
			return;
		case TelehashPackage.LINE__SEEN_AT:
			setSeenAt((Long) newValue);
			return;
		case TelehashPackage.LINE__SENT_AT:
			setSentAt((Long) newValue);
			return;
		case TelehashPackage.LINE__LINE_AT:
			setLineAt((Long) newValue);
			return;
		case TelehashPackage.LINE__TAP_LAST_AT:
			setTapLastAt((Integer) newValue);
			return;
		case TelehashPackage.LINE__BR:
			setBr((Integer) newValue);
			return;
		case TelehashPackage.LINE__BR_IN:
			setBrIn((Integer) newValue);
			return;
		case TelehashPackage.LINE__BR_OUT:
			setBrOut((Integer) newValue);
			return;
		case TelehashPackage.LINE__BSENT:
			setBsent((Integer) newValue);
			return;
		case TelehashPackage.LINE__LINE_ID:
			setLineId((Integer) newValue);
			return;
		case TelehashPackage.LINE__VISIBLE:
			setVisible((Boolean) newValue);
			return;
		case TelehashPackage.LINE__ADVERTISED:
			setAdvertised((Boolean) newValue);
			return;
		case TelehashPackage.LINE__RULES:
			getRules().clear();
			getRules().addAll((Collection<? extends TapRule>) newValue);
			return;
		case TelehashPackage.LINE__SESSION:
			setSession((IoSession) newValue);
			return;
		}
		super.eSet(featureID, newValue);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void eUnset(int featureID) {
		switch (featureID) {
		case TelehashPackage.LINE__ADDRESS:
			setAddress(ADDRESS_EDEFAULT);
			return;
		case TelehashPackage.LINE__END:
			setEnd(END_EDEFAULT);
			return;
		case TelehashPackage.LINE__NEIGHBORS:
			getNeighbors().clear();
			return;
		case TelehashPackage.LINE__RING_IN:
			unsetRingIn();
			return;
		case TelehashPackage.LINE__RING_OUT:
			unsetRingOut();
			return;
		case TelehashPackage.LINE__INIT:
			unsetInit();
			return;
		case TelehashPackage.LINE__SEEN_AT:
			unsetSeenAt();
			return;
		case TelehashPackage.LINE__SENT_AT:
			unsetSentAt();
			return;
		case TelehashPackage.LINE__LINE_AT:
			unsetLineAt();
			return;
		case TelehashPackage.LINE__TAP_LAST_AT:
			unsetTapLastAt();
			return;
		case TelehashPackage.LINE__BR:
			setBr(BR_EDEFAULT);
			return;
		case TelehashPackage.LINE__BR_IN:
			setBrIn(BR_IN_EDEFAULT);
			return;
		case TelehashPackage.LINE__BR_OUT:
			setBrOut(BR_OUT_EDEFAULT);
			return;
		case TelehashPackage.LINE__BSENT:
			setBsent(BSENT_EDEFAULT);
			return;
		case TelehashPackage.LINE__LINE_ID:
			unsetLineId();
			return;
		case TelehashPackage.LINE__VISIBLE:
			setVisible(VISIBLE_EDEFAULT);
			return;
		case TelehashPackage.LINE__ADVERTISED:
			setAdvertised(ADVERTISED_EDEFAULT);
			return;
		case TelehashPackage.LINE__RULES:
			unsetRules();
			return;
		case TelehashPackage.LINE__SESSION:
			setSession(SESSION_EDEFAULT);
			return;
		}
		super.eUnset(featureID);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean eIsSet(int featureID) {
		switch (featureID) {
		case TelehashPackage.LINE__ADDRESS:
			return ADDRESS_EDEFAULT == null ? address != null
					: !ADDRESS_EDEFAULT.equals(address);
		case TelehashPackage.LINE__END:
			return END_EDEFAULT == null ? end != null : !END_EDEFAULT
					.equals(end);
		case TelehashPackage.LINE__NEIGHBORS:
			return neighbors != null && !neighbors.isEmpty();
		case TelehashPackage.LINE__RING_IN:
			return isSetRingIn();
		case TelehashPackage.LINE__RING_OUT:
			return isSetRingOut();
		case TelehashPackage.LINE__INIT:
			return isSetInit();
		case TelehashPackage.LINE__SEEN_AT:
			return isSetSeenAt();
		case TelehashPackage.LINE__SENT_AT:
			return isSetSentAt();
		case TelehashPackage.LINE__LINE_AT:
			return isSetLineAt();
		case TelehashPackage.LINE__TAP_LAST_AT:
			return isSetTapLastAt();
		case TelehashPackage.LINE__BR:
			return br != BR_EDEFAULT;
		case TelehashPackage.LINE__BR_IN:
			return brIn != BR_IN_EDEFAULT;
		case TelehashPackage.LINE__BR_OUT:
			return brOut != BR_OUT_EDEFAULT;
		case TelehashPackage.LINE__BSENT:
			return bsent != BSENT_EDEFAULT;
		case TelehashPackage.LINE__LINE_ID:
			return isSetLineId();
		case TelehashPackage.LINE__VISIBLE:
			return visible != VISIBLE_EDEFAULT;
		case TelehashPackage.LINE__ADVERTISED:
			return advertised != ADVERTISED_EDEFAULT;
		case TelehashPackage.LINE__RULES:
			return isSetRules();
		case TelehashPackage.LINE__SESSION:
			return SESSION_EDEFAULT == null ? session != null
					: !SESSION_EDEFAULT.equals(session);
		}
		return super.eIsSet(featureID);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public String toString() {
		if (eIsProxy())
			return super.toString();

		StringBuffer result = new StringBuffer(super.toString());
		result.append(" (address: ");
		result.append(address);
		result.append(", end: ");
		result.append(end);
		result.append(", neighbors: ");
		result.append(neighbors);
		result.append(", ringIn: ");
		if (ringInESet)
			result.append(ringIn);
		else
			result.append("<unset>");
		result.append(", ringOut: ");
		if (ringOutESet)
			result.append(ringOut);
		else
			result.append("<unset>");
		result.append(", init: ");
		if (initESet)
			result.append(init);
		else
			result.append("<unset>");
		result.append(", seenAt: ");
		if (seenAtESet)
			result.append(seenAt);
		else
			result.append("<unset>");
		result.append(", sentAt: ");
		if (sentAtESet)
			result.append(sentAt);
		else
			result.append("<unset>");
		result.append(", lineAt: ");
		if (lineAtESet)
			result.append(lineAt);
		else
			result.append("<unset>");
		result.append(", tapLastAt: ");
		if (tapLastAtESet)
			result.append(tapLastAt);
		else
			result.append("<unset>");
		result.append(", br: ");
		result.append(br);
		result.append(", brIn: ");
		result.append(brIn);
		result.append(", brOut: ");
		result.append(brOut);
		result.append(", bsent: ");
		result.append(bsent);
		result.append(", lineId: ");
		if (lineIdESet)
			result.append(lineId);
		else
			result.append("<unset>");
		result.append(", visible: ");
		result.append(visible);
		result.append(", advertised: ");
		result.append(advertised);
		result.append(", session: ");
		result.append(session);
		result.append(')');
		return result.toString();
	}

} //LineImpl
