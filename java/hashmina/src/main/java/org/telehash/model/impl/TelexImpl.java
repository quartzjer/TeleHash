/**
 * <copyright>
 * </copyright>
 *
 * $Id$
 */
package org.telehash.model.impl;

import java.net.InetSocketAddress;

import java.util.Collection;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EClass;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;

import org.eclipse.emf.ecore.util.EDataTypeUniqueEList;

import org.telehash.Hash;

import org.telehash.model.TelehashPackage;
import org.telehash.model.Telex;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Telex</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.telehash.model.impl.TelexImpl#getTo <em>To</em>}</li>
 *   <li>{@link org.telehash.model.impl.TelexImpl#getEnd <em>End</em>}</li>
 *   <li>{@link org.telehash.model.impl.TelexImpl#getLine <em>Line</em>}</li>
 *   <li>{@link org.telehash.model.impl.TelexImpl#getRing <em>Ring</em>}</li>
 *   <li>{@link org.telehash.model.impl.TelexImpl#getSee <em>See</em>}</li>
 *   <li>{@link org.telehash.model.impl.TelexImpl#getBytesReceived <em>Bytes Received</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class TelexImpl extends EObjectImpl implements Telex {
	/**
	 * The default value of the '{@link #getTo() <em>To</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getTo()
	 * @generated
	 * @ordered
	 */
	protected static final InetSocketAddress TO_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getTo() <em>To</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getTo()
	 * @generated
	 * @ordered
	 */
	protected InetSocketAddress to = TO_EDEFAULT;

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
	 * The default value of the '{@link #getLine() <em>Line</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getLine()
	 * @generated
	 * @ordered
	 */
	protected static final int LINE_EDEFAULT = 0;

	/**
	 * The cached value of the '{@link #getLine() <em>Line</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getLine()
	 * @generated
	 * @ordered
	 */
	protected int line = LINE_EDEFAULT;

	/**
	 * This is true if the Line attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean lineESet;

	/**
	 * The default value of the '{@link #getRing() <em>Ring</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getRing()
	 * @generated
	 * @ordered
	 */
	protected static final int RING_EDEFAULT = 0;

	/**
	 * The cached value of the '{@link #getRing() <em>Ring</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getRing()
	 * @generated
	 * @ordered
	 */
	protected int ring = RING_EDEFAULT;

	/**
	 * This is true if the Ring attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean ringESet;

	/**
	 * The cached value of the '{@link #getSee() <em>See</em>}' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getSee()
	 * @generated
	 * @ordered
	 */
	protected EList<InetSocketAddress> see;

	/**
	 * The default value of the '{@link #getBytesReceived() <em>Bytes Received</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getBytesReceived()
	 * @generated
	 * @ordered
	 */
	protected static final int BYTES_RECEIVED_EDEFAULT = 0;

	/**
	 * The cached value of the '{@link #getBytesReceived() <em>Bytes Received</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getBytesReceived()
	 * @generated
	 * @ordered
	 */
	protected int bytesReceived = BYTES_RECEIVED_EDEFAULT;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected TelexImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return TelehashPackage.Literals.TELEX;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public InetSocketAddress getTo() {
		return to;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setTo(InetSocketAddress newTo) {
		InetSocketAddress oldTo = to;
		to = newTo;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					TelehashPackage.TELEX__TO, oldTo, to));
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
	public void setEnd(Hash newEnd) {
		Hash oldEnd = end;
		end = newEnd;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					TelehashPackage.TELEX__END, oldEnd, end));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public int getLine() {
		return line;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setLine(int newLine) {
		int oldLine = line;
		line = newLine;
		boolean oldLineESet = lineESet;
		lineESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					TelehashPackage.TELEX__LINE, oldLine, line, !oldLineESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetLine() {
		int oldLine = line;
		boolean oldLineESet = lineESet;
		line = LINE_EDEFAULT;
		lineESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET,
					TelehashPackage.TELEX__LINE, oldLine, LINE_EDEFAULT,
					oldLineESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetLine() {
		return lineESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public int getRing() {
		return ring;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setRing(int newRing) {
		int oldRing = ring;
		ring = newRing;
		boolean oldRingESet = ringESet;
		ringESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					TelehashPackage.TELEX__RING, oldRing, ring, !oldRingESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetRing() {
		int oldRing = ring;
		boolean oldRingESet = ringESet;
		ring = RING_EDEFAULT;
		ringESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET,
					TelehashPackage.TELEX__RING, oldRing, RING_EDEFAULT,
					oldRingESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetRing() {
		return ringESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList<InetSocketAddress> getSee() {
		if (see == null) {
			see = new EDataTypeUniqueEList<InetSocketAddress>(
					InetSocketAddress.class, this, TelehashPackage.TELEX__SEE);
		}
		return see;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public int getBytesReceived() {
		return bytesReceived;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setBytesReceived(int newBytesReceived) {
		int oldBytesReceived = bytesReceived;
		bytesReceived = newBytesReceived;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					TelehashPackage.TELEX__BYTES_RECEIVED, oldBytesReceived,
					bytesReceived));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
		case TelehashPackage.TELEX__TO:
			return getTo();
		case TelehashPackage.TELEX__END:
			return getEnd();
		case TelehashPackage.TELEX__LINE:
			return getLine();
		case TelehashPackage.TELEX__RING:
			return getRing();
		case TelehashPackage.TELEX__SEE:
			return getSee();
		case TelehashPackage.TELEX__BYTES_RECEIVED:
			return getBytesReceived();
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
		case TelehashPackage.TELEX__TO:
			setTo((InetSocketAddress) newValue);
			return;
		case TelehashPackage.TELEX__END:
			setEnd((Hash) newValue);
			return;
		case TelehashPackage.TELEX__LINE:
			setLine((Integer) newValue);
			return;
		case TelehashPackage.TELEX__RING:
			setRing((Integer) newValue);
			return;
		case TelehashPackage.TELEX__SEE:
			getSee().clear();
			getSee().addAll((Collection<? extends InetSocketAddress>) newValue);
			return;
		case TelehashPackage.TELEX__BYTES_RECEIVED:
			setBytesReceived((Integer) newValue);
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
		case TelehashPackage.TELEX__TO:
			setTo(TO_EDEFAULT);
			return;
		case TelehashPackage.TELEX__END:
			setEnd(END_EDEFAULT);
			return;
		case TelehashPackage.TELEX__LINE:
			unsetLine();
			return;
		case TelehashPackage.TELEX__RING:
			unsetRing();
			return;
		case TelehashPackage.TELEX__SEE:
			getSee().clear();
			return;
		case TelehashPackage.TELEX__BYTES_RECEIVED:
			setBytesReceived(BYTES_RECEIVED_EDEFAULT);
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
		case TelehashPackage.TELEX__TO:
			return TO_EDEFAULT == null ? to != null : !TO_EDEFAULT.equals(to);
		case TelehashPackage.TELEX__END:
			return END_EDEFAULT == null ? end != null : !END_EDEFAULT
					.equals(end);
		case TelehashPackage.TELEX__LINE:
			return isSetLine();
		case TelehashPackage.TELEX__RING:
			return isSetRing();
		case TelehashPackage.TELEX__SEE:
			return see != null && !see.isEmpty();
		case TelehashPackage.TELEX__BYTES_RECEIVED:
			return bytesReceived != BYTES_RECEIVED_EDEFAULT;
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
		result.append(" (to: ");
		result.append(to);
		result.append(", end: ");
		result.append(end);
		result.append(", line: ");
		if (lineESet)
			result.append(line);
		else
			result.append("<unset>");
		result.append(", ring: ");
		if (ringESet)
			result.append(ring);
		else
			result.append("<unset>");
		result.append(", see: ");
		result.append(see);
		result.append(", bytesReceived: ");
		result.append(bytesReceived);
		result.append(')');
		return result.toString();
	}

} //TelexImpl
