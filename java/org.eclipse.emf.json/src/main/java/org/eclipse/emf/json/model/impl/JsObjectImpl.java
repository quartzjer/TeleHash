/**
 * <copyright>
 * </copyright>
 *
 * $Id$
 */
package org.eclipse.emf.json.model.impl;

import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.common.util.EMap;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.EObjectImpl;
import org.eclipse.emf.ecore.util.EcoreEMap;
import org.eclipse.emf.ecore.util.InternalEList;
import org.eclipse.emf.json.model.JsObject;
import org.eclipse.emf.json.model.JsonPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Js Object</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.eclipse.emf.json.model.impl.JsObjectImpl#getUnmatched <em>Unmatched</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class JsObjectImpl extends EObjectImpl implements JsObject {
	/**
	 * The cached value of the '{@link #getUnmatched() <em>Unmatched</em>}' map.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getUnmatched()
	 * @generated
	 * @ordered
	 */
	protected EMap<String, Object> unmatched;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected JsObjectImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return JsonPackage.Literals.JS_OBJECT;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EMap<String, Object> getUnmatched() {
		if (unmatched == null) {
			unmatched = new EcoreEMap<String,Object>(JsonPackage.Literals.ESTRING_TO_ANY_SIMPLE_TYPE_MAP, EStringToAnySimpleTypeMapImpl.class, this, JsonPackage.JS_OBJECT__UNMATCHED);
		}
		return unmatched;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Object get(String key) {
		// TODO: implement this method
		// Ensure that you remove @generated or mark it @generated NOT
		throw new UnsupportedOperationException();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case JsonPackage.JS_OBJECT__UNMATCHED:
				return ((InternalEList<?>)getUnmatched()).basicRemove(otherEnd, msgs);
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
			case JsonPackage.JS_OBJECT__UNMATCHED:
				if (coreType) return getUnmatched();
				else return getUnmatched().map();
		}
		return super.eGet(featureID, resolve, coreType);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case JsonPackage.JS_OBJECT__UNMATCHED:
				((EStructuralFeature.Setting)getUnmatched()).set(newValue);
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
			case JsonPackage.JS_OBJECT__UNMATCHED:
				getUnmatched().clear();
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
			case JsonPackage.JS_OBJECT__UNMATCHED:
				return unmatched != null && !unmatched.isEmpty();
		}
		return super.eIsSet(featureID);
	}

} //JsObjectImpl
