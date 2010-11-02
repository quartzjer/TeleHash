/**
 * <copyright>
 * </copyright>
 *
 * $Id$
 */
package org.telehash.model.impl;

import java.util.Collection;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.util.EDataTypeUniqueEList;
import org.eclipse.emf.ecore.util.InternalEList;
import org.eclipse.emf.json.model.JsObject;
import org.eclipse.emf.json.model.impl.JsObjectImpl;
import org.telehash.model.TapRule;
import org.telehash.model.TelehashPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Tap Rule</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.telehash.model.impl.TapRuleImpl#getIs <em>Is</em>}</li>
 *   <li>{@link org.telehash.model.impl.TapRuleImpl#getHas <em>Has</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class TapRuleImpl extends JsObjectImpl implements TapRule {
	/**
	 * The cached value of the '{@link #getIs() <em>Is</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getIs()
	 * @generated
	 * @ordered
	 */
	protected JsObject is;

	/**
	 * This is true if the Is containment reference has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean isESet;

	/**
	 * The cached value of the '{@link #getHas() <em>Has</em>}' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getHas()
	 * @generated
	 * @ordered
	 */
	protected EList<String> has;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected TapRuleImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return TelehashPackage.Literals.TAP_RULE;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public JsObject getIs() {
		return is;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public NotificationChain basicSetIs(JsObject newIs, NotificationChain msgs) {
		JsObject oldIs = is;
		is = newIs;
		boolean oldIsESet = isESet;
		isESet = true;
		if (eNotificationRequired()) {
			ENotificationImpl notification = new ENotificationImpl(this,
					Notification.SET, TelehashPackage.TAP_RULE__IS, oldIs,
					newIs, !oldIsESet);
			if (msgs == null)
				msgs = notification;
			else
				msgs.add(notification);
		}
		return msgs;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public TapRule withIs(JsObject value) {
		setIs(value);
		return this;
	}

	public void setIs(JsObject newIs) {
		if (newIs != is) {
			NotificationChain msgs = null;
			if (is != null)
				msgs = ((InternalEObject) is).eInverseRemove(this,
						EOPPOSITE_FEATURE_BASE - TelehashPackage.TAP_RULE__IS,
						null, msgs);
			if (newIs != null)
				msgs = ((InternalEObject) newIs).eInverseAdd(this,
						EOPPOSITE_FEATURE_BASE - TelehashPackage.TAP_RULE__IS,
						null, msgs);
			msgs = basicSetIs(newIs, msgs);
			if (msgs != null)
				msgs.dispatch();
		} else {
			boolean oldIsESet = isESet;
			isESet = true;
			if (eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.SET,
						TelehashPackage.TAP_RULE__IS, newIs, newIs, !oldIsESet));
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public NotificationChain basicUnsetIs(NotificationChain msgs) {
		JsObject oldIs = is;
		is = null;
		boolean oldIsESet = isESet;
		isESet = false;
		if (eNotificationRequired()) {
			ENotificationImpl notification = new ENotificationImpl(this,
					Notification.UNSET, TelehashPackage.TAP_RULE__IS, oldIs,
					null, oldIsESet);
			if (msgs == null)
				msgs = notification;
			else
				msgs.add(notification);
		}
		return msgs;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetIs() {
		if (is != null) {
			NotificationChain msgs = null;
			msgs = ((InternalEObject) is).eInverseRemove(this,
					EOPPOSITE_FEATURE_BASE - TelehashPackage.TAP_RULE__IS,
					null, msgs);
			msgs = basicUnsetIs(msgs);
			if (msgs != null)
				msgs.dispatch();
		} else {
			boolean oldIsESet = isESet;
			isESet = false;
			if (eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.UNSET,
						TelehashPackage.TAP_RULE__IS, null, null, oldIsESet));
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetIs() {
		return isESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList<String> getHas() {
		if (has == null) {
			has = new EDataTypeUniqueEList.Unsettable<String>(String.class,
					this, TelehashPackage.TAP_RULE__HAS);
		}
		return has;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetHas() {
		if (has != null)
			((InternalEList.Unsettable<?>) has).unset();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetHas() {
		return has != null && ((InternalEList.Unsettable<?>) has).isSet();
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
		case TelehashPackage.TAP_RULE__IS:
			return basicUnsetIs(msgs);
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
		case TelehashPackage.TAP_RULE__IS:
			return getIs();
		case TelehashPackage.TAP_RULE__HAS:
			return getHas();
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
		case TelehashPackage.TAP_RULE__IS:
			setIs((JsObject) newValue);
			return;
		case TelehashPackage.TAP_RULE__HAS:
			getHas().clear();
			getHas().addAll((Collection<? extends String>) newValue);
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
		case TelehashPackage.TAP_RULE__IS:
			unsetIs();
			return;
		case TelehashPackage.TAP_RULE__HAS:
			unsetHas();
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
		case TelehashPackage.TAP_RULE__IS:
			return isSetIs();
		case TelehashPackage.TAP_RULE__HAS:
			return isSetHas();
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
		result.append(" (has: ");
		result.append(has);
		result.append(')');
		return result.toString();
	}

} //TapRuleImpl
