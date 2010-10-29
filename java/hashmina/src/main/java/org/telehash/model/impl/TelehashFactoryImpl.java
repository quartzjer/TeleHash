/**
 * <copyright>
 * </copyright>
 *
 * $Id$
 */
package org.telehash.model.impl;

import java.net.InetSocketAddress;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EDataType;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.impl.EFactoryImpl;
import org.eclipse.emf.ecore.plugin.EcorePlugin;
import org.telehash.Hash;
import org.telehash.model.TelehashFactory;
import org.telehash.model.TelehashPackage;
import org.telehash.model.Telex;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model <b>Factory</b>.
 * <!-- end-user-doc -->
 * @generated
 */
public class TelehashFactoryImpl extends EFactoryImpl implements
		TelehashFactory {
	/**
	 * Creates the default factory implementation.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static TelehashFactory init() {
		try {
			TelehashFactory theTelehashFactory = (TelehashFactory) EPackage.Registry.INSTANCE
					.getEFactory("http://telehash.org/ecore/2010");
			if (theTelehashFactory != null) {
				return theTelehashFactory;
			}
		} catch (Exception exception) {
			EcorePlugin.INSTANCE.log(exception);
		}
		return new TelehashFactoryImpl();
	}

	/**
	 * Creates an instance of the factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public TelehashFactoryImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EObject create(EClass eClass) {
		switch (eClass.getClassifierID()) {
		case TelehashPackage.TELEX:
			return createTelex();
		default:
			throw new IllegalArgumentException("The class '" + eClass.getName()
					+ "' is not a valid classifier");
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object createFromString(EDataType eDataType, String initialValue) {
		switch (eDataType.getClassifierID()) {
		case TelehashPackage.ENDPOINT:
			return createEndpointFromString(eDataType, initialValue);
		case TelehashPackage.HASH:
			return createHashFromString(eDataType, initialValue);
		default:
			throw new IllegalArgumentException("The datatype '"
					+ eDataType.getName() + "' is not a valid classifier");
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public String convertToString(EDataType eDataType, Object instanceValue) {
		switch (eDataType.getClassifierID()) {
		case TelehashPackage.ENDPOINT:
			return convertEndpointToString(eDataType, instanceValue);
		case TelehashPackage.HASH:
			return convertHashToString(eDataType, instanceValue);
		default:
			throw new IllegalArgumentException("The datatype '"
					+ eDataType.getName() + "' is not a valid classifier");
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Telex createTelex() {
		TelexImpl telex = new TelexImpl();
		return telex;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated NOT
	 */
	public InetSocketAddress createEndpointFromString(EDataType eDataType,
			String initialValue) {
		String[] addrFields = initialValue.split(":");
		return new InetSocketAddress(addrFields[0],
				Integer.parseInt(addrFields[1]));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated NOT
	 */
	public String convertEndpointToString(EDataType eDataType,
			Object instanceValue) {
		InetSocketAddress addr = (InetSocketAddress) instanceValue;
		return addr.getAddress().getHostAddress() + ":" + addr.getPort();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated NOT
	 */
	public Hash createHashFromString(EDataType eDataType, String initialValue) {
		return new Hash(initialValue);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String convertHashToString(EDataType eDataType, Object instanceValue) {
		return super.convertToString(eDataType, instanceValue);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public TelehashPackage getTelehashPackage() {
		return (TelehashPackage) getEPackage();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @deprecated
	 * @generated
	 */
	@Deprecated
	public static TelehashPackage getPackage() {
		return TelehashPackage.eINSTANCE;
	}

} //TelehashFactoryImpl
